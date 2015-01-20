module Hash (
  runInteractive,
  runScript
) where

import Parsing.HashParser
import Language.Exec
import Control.Monad (when)
import Language.Commands (command_tb, CommandTable, ScriptState(..))
import Language.Expressions
import System.Directory
import System.IO
import qualified Data.Map as M
import Data.Maybe
import Data.List (isPrefixOf)

runInteractive :: IO ()
runInteractive = do
  currDir <- getCurrentDirectory
  let ss = ScriptState "" currDir $ M.fromList [("HOME", currDir)]
  runInteractiveIter command_tb ss
  return ()
  
runInteractiveIter :: CommandTable -> ScriptState -> IO ScriptState
runInteractiveIter ct ss = do
  putStr "> "
  hFlush stdout
  string <- getLine
  if not $ isPrefixOf "quit" string then do
    let cmdLine = simpleParse readCommandLine string
    cmd <- case cmdLine of
             Left err -> do
                           putStrLn $ show err
                           return Nothing
             Right ss -> return $ Just ss
    nss <- if isJust cmd then
             runTopLevel command_tb ss (TLCmd (fromJust cmd))
           else
             return ss
    runInteractiveIter ct nss
  else
    return ss
    
runScript :: FilePath -> IO ()
runScript path = do
  handle <- openFile path ReadMode
  cont   <- hGetContents handle
  coms <- case simpleParse tlexpressions cont of
             Left err -> do
                           putStrLn $ show err
                           return Nothing
             Right ss -> return $ Just ss
  when (isJust coms) $ do
    nss <- runHashProgram command_tb (Left "script_state.conf") (fromJust coms)
    putStrLn $ show nss
  return ()