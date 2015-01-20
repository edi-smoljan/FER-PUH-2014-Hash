module Language.Commands (
  Command,
  VarTable,
  CommandTable,
  ScriptState(..),
  command_tb
) where

import System.Directory
import System.FilePath.Posix
import System.Environment  
import System.IO  
import System.IO.Error (ioeGetFileName, ioeGetErrorString, userError)
import Control.Exception (catch, throw)
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Control.Monad (when, forM_)
import Control.Applicative ((<$>), (<*>))
import Data.Char (isSpace, toUpper, chr, isPrint)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Data.Foldable (foldlM)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Word as W
import Numeric (showHex)

type Command = [String] -> ScriptState -> Handle -> Handle -> IO ScriptState
type VarTable = M.Map String String
type CommandTable = M.Map String Command
data ScriptState = ScriptState { output :: String
                               , wd :: FilePath
                               , vartable :: VarTable
                               } deriving Show
                               
                               
command_tb :: CommandTable
command_tb = M.fromList [("echo", echo), ("cp", cp), ("mv", mv),
                         ("rm", rm), ("rmdir", rmdir), ("mkdir", mkdir),
                         ("cpdir", cpdir), ("mvdir", mvdir), ("ls", ls),
                         ("pwd", pwd), ("cd", cd), ("cat", cat), ("create", create),
                         ("hexdump", hexdump)]
    
mvcpTemp :: (Command) -> Command
mvcpTemp fun args ss input output = do
  let len       = length args
      normPaths = normalizePaths ss args
  if len < 2 then do
    hPutStrLn output "At least two arguments expected!"
    return ss
  else if len == 2 && isTarget (args !! 1) then do
    (fun normPaths ss input output)
  else do
    let destDir       = last normPaths
        srcFilePaths  = init normPaths
        destFilePaths = map ((destDir </>) . takeFileName) srcFilePaths
        srcDestPairs  = zip srcFilePaths destFilePaths
    foldlM (\ssa (src,dst) -> fun [src, dst] ssa input output) ss srcDestPairs

errorHandler :: Handle -> ScriptState -> IOError -> IO (ScriptState)
errorHandler output ss e = do
  let filepath = case ioeGetFileName e of Just s  -> s
                                          Nothing -> ""
      errorMes = ioeGetErrorString e
      message  = filepath ++ " : " ++ errorMes
      nss      = ss {output=message}
  hPutStrLn output message
  return nss
        
cpHelp :: Command
cpHelp [src, target] ss input output = do
  (do copyFile src target
      return ss) `catch` (errorHandler output ss)
  
cp :: Command
cp = mvcpTemp cpHelp

mvHelp :: Command
mvHelp [src, target] ss input output = do
  (do copyFile src target
      removeFile src
      return ss)  `catch` (errorHandler output ss)
 
mv :: Command
mv = mvcpTemp mvHelp
       
rmcrTemplate :: (String -> IO ()) -> Command
rmcrTemplate fun args ss input output = do
  let normPaths = normalizePaths ss args
  foldlM (\ssa src -> (do fun src
                          return ssa) `catch` (errorHandler output ssa)) ss normPaths

rm :: Command
rm = rmcrTemplate removeFile
  
create :: Command
create = rmcrTemplate createFile

createFile :: FilePath -> IO ()
createFile path = do
  let dir  = takeDirectory path
      name = takeFileName path
  (path2, hnd) <- openTempFile dir name
  hClose hnd
  renameFile path2 path
  

rmdir :: Command
rmdir = rmcrTemplate removeDirectory

mkdir :: Command
mkdir = rmcrTemplate $ createDirectoryIfMissing True

cpdirHelp :: [FilePath] -> IO ()
cpdirHelp [src, dest] = do
  srcExist  <- doesDirectoryExist src
  destExist <- (||) <$> doesDirectoryExist dest <*> doesFileExist dest
  if not srcExist then
    throw (userError $ "Source dir " ++ src  ++ " does not exist")
  else if destExist then
    throw (userError $ "Destination file / dir " ++ dest ++ " already exists")
  else do
    createDirectory dest
    contents <- getDirectoryContents src
    let filteredCont     = filter (\s -> s `notElem` [".", ".."]) contents
        srcContentPaths  = map (src </>) filteredCont
        destContentPaths = map (dest </>) filteredCont
    forM_ (zip srcContentPaths destContentPaths) $ \(srcEnt, destEnt) -> do
      isFileSrc <- doesFileExist srcEnt
      isDirSrc  <- doesDirectoryExist srcEnt
      if isFileSrc then 
        copyFile srcEnt destEnt
      else if isDirSrc then 
        cpdirHelp [srcEnt, destEnt]
      else do
        throw (userError $ "Content " ++ srcEnt ++ " not directory or file")
    return ()
    
cpdir :: Command
cpdir paths ss input output = do
  let dst  = last normPaths
      srcs = init normPaths
      normPaths = normalizePaths ss paths 
  foldlM (\ssa src -> ( do cpdirHelp [src, dst]
                           return ssa ) `catch` (errorHandler output ssa)) ss srcs

mvdirHelp :: [FilePath] -> Handle -> ScriptState -> IO ScriptState
mvdirHelp [src, dest] output ss = do
  (do cpdirHelp [src, dest]
      removeDirectoryRecursive src
      return ss) `catch` (errorHandler output ss)
  
mvdir :: Command
mvdir paths ss input output = do
  let dst  = last normPaths
      srcs = init normPaths
      normPaths = normalizePaths ss paths 
  foldlM (\ssa src -> mvdirHelp [src, dst] output ssa) ss srcs

echo :: Command
echo args ss input output = do
  let normalizedStr = intercalate " " args 
  hPutStrLn output $ normalizedStr
  return ss{output=normalizedStr}
  
ls :: Command
ls args ss input output = do
  let normPaths = normalizePaths ss args
      dir       = if null normPaths then wd ss else head normPaths
  dirExist  <- doesDirectoryExist dir
  if dirExist then do
    contents <- getDirectoryContents dir
    let message = intercalate "\n" contents
    hPutStrLn output message
    return ss{output=message}
  else do
    let message = "Directory " ++ dir ++ "doesn't exist"
    hPutStrLn output message
    return ss{output=message}
  
pwd :: Command
pwd args ss input output = do
  let message = wd ss
  hPutStrLn output message
  return ss{output=message}
  
cd :: Command
cd args ss input output = do
  let normPaths = normalizePaths ss args
      dir = if null normPaths then userDir else head normPaths
      userDir = fromJust . M.lookup "HOME" . vartable $ ss
  dirExist <- doesDirectoryExist dir
  if dirExist then
    return ss{wd=dir}
  else do
    let message = "Directory " ++ dir ++ "doesn't exist"
    hPutStrLn output message
    return ss{output=message}
    
cat :: Command
cat args ss input output = do
  let normPaths = normalizePaths ss args
  foldlM (\ssa path -> (printFile path output ssa) 
    `catch` (errorHandler output ssa)) ss normPaths
  
printFile :: FilePath -> Handle -> ScriptState -> IO ScriptState
printFile path output ss = do
  file <- openFile path ReadMode
  contents <- hGetContents file
  hPutStrLn output contents
  hClose file
  return ss{output=contents}
  
hexdump :: Command
hexdump args ss input output = do
  let normPaths = normalizePaths ss args
  (do bytes <- BS.readFile $ head args
      printDump ss bytes) `catch` (errorHandler output ss)
  
printDump :: ScriptState -> BS.ByteString -> IO ScriptState
printDump ss stream =
  if not (BS.null stream) then do
    let toPrint   = BS.unpack $ BS.take 16 stream
        hexString = intercalate " " . map convertToHexString $ toPrint
        numSpaces = 5 + (16 - (length toPrint)) * 3 - 1
        printed   = hexString ++ (replicate numSpaces ' ') ++ (map toPrintChar toPrint)
    putStrLn printed
    printDump (ss{output=printed}) (BS.drop 16 stream)
  else
    return ss
    
convertToHexString :: W.Word8 -> String
convertToHexString byte = if (length str) == 1 then '0':str else str
  where str = map toUpper $ showHex byte ""

toPrintChar :: W.Word8 -> Char
toPrintChar byte = if isPrint char then char else '.'
  where char = chr $ fromIntegral byte

{-Util functions-}
isTarget :: FilePath -> Bool
isTarget path = not $ isSuffixOf "/" path

normalizePaths :: ScriptState -> [String] -> [String]
normalizePaths ss paths = map appendWD paths
  where workingDir = wd ss
        appendWD path = if isRelative path then 
                          workingDir </> path 
                        else 
                          path