module Language.Exec (
  runHashProgram,
  runTopLevel
) where

import qualified Data.Map as M
import System.IO
import Parsing.HashParser
import Language.Commands
import Language.Expressions
import System.Directory
import System.FilePath.Posix
import Data.Maybe (isNothing, fromJust)
import Control.Monad (when, forM_, foldM)
import Data.Char (isSpace)
                               
                               
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram table (Left path) exprs = do
  handle <- openFile path ReadMode
  cont   <- hGetContents handle
  curr   <- getCurrentDirectory
  let parsed = simpleParse readCommandBlock cont
  tableV <- case parsed of
              Left pe -> do
                putStrLn $ show pe
                return M.empty
              Right a ->
                return $ M.fromList . map (\(Assign var val) -> (var, val)) $ a
  let ss     = ScriptState {output="", wd=curr, vartable=tableV}
  hClose handle
  runHashProgram table (Right ss) exprs
  
runHashProgram table (Right ss) [] = return ss

runHashProgram table (Right ss) (expr:other) = do
  nss <- runTopLevel table ss expr
  runHashProgram table (Right nss) other
  
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel table ss (TLCmd (Cmd name args input output append)) = do
  let inputFile  = extractPath input
      outputFile = extractPath output
      normArgs   = map (flip normalize $ vartable ss) args
  executeCommand (normalize name $ vartable ss) normArgs inputFile outputFile append ss
  where extractPath Nothing  = ""
        extractPath (Just p) = head $ normalizePaths ss [p]
      
      
runTopLevel table ss (TLCmd (Assign var val)) = do
  let vart    = vartable ss
      normVal = normalize val vart
  return ss{vartable=(M.insert var normVal vart)}
  
runTopLevel table ss (TLCmd (Comment _)) = return ss

runTopLevel table ss (TLConditional (If pred cthen)) = do
  if evaluate pred (vartable ss) then
    foldM (\acc x -> runTopLevel table acc (TLCmd x)) ss cthen
  else
    return ss 
    
runTopLevel table ss (TLConditional (IfElse pred cthen celse)) = do
  if evaluate pred (vartable ss) then
    foldM (\acc x -> runTopLevel table acc (TLCmd x)) ss cthen
  else
    foldM (\acc x -> runTopLevel table acc (TLCmd x)) ss celse
    
runTopLevel table ss (TLLoop (For var vals cmds)) = forLoopin table ss varNorm valsNorm cmds
  where varNorm = normalize var (vartable ss)
        valsNorm = map ((flip normalize) $ vartable ss) vals

forLoopin :: CommandTable -> ScriptState -> String -> [String] -> [Cmd] -> IO ScriptState
forLoopin _ ss _ [] _                   = return ss
forLoopin ctable ss var (val:rest) cmds = do
  let ss1 = ss{vartable=(M.insert var val $ vartable ss)}
  ss2 <- foldM (\acc x -> runTopLevel ctable acc (TLCmd x)) ss1 cmds
  forLoopin ctable ss2 var rest cmds
      
--Util functions

evaluate' :: Comp -> VarTable -> Bool
evaluate' (CEQ s1 s2) vt = (normalize s1 vt) == (normalize s2 vt)
evaluate' (CNE s1 s2) vt = (normalize s1 vt) /= (normalize s2 vt)
evaluate' (CGE s1 s2) vt = (normalize s1 vt) >= (normalize s2 vt)
evaluate' (CGT s1 s2) vt = (normalize s1 vt) > (normalize s2 vt)
evaluate' (CLE s1 s2) vt = (normalize s1 vt) <= (normalize s2 vt)
evaluate' (CLT s1 s2) vt = (normalize s1 vt) < (normalize s2 vt)
evaluate' (CLI s1)    vt = not . null $ normalize s1 vt

evaluate :: Pred -> VarTable -> Bool
evaluate (Pred comp) vt       = evaluate' comp vt
evaluate (Not pred) vt        = not $ evaluate pred vt
evaluate (And pred1 pred2) vt = (evaluate pred1 vt) && (evaluate pred2 vt)
evaluate (Or pred1 pred2) vt  = (evaluate pred1 vt) || (evaluate pred2 vt)
evaluate (Parens pred1) vt    = evaluate pred1 vt

--normalizes the command line arguments (evaluates the variables, converts the paths to absolute and so on
normalize :: String -> VarTable -> String
normalize arg table
  | not toEvaluateVars = stripQuotation arg
  | otherwise          = skipChars . evaluateVars table 0 . stripQuotation $ arg
  where toEvaluateVars = (/=) '\'' $ head arg
        
stripQuotation arg@(c:rest)
  | c == '"'   = if last rest == '"'  then init rest else arg
  | c == '\''  = if last rest == '\'' then init rest else arg
  | otherwise  = arg
        
evaluateVars :: VarTable -> Int -> String -> String
evaluateVars _ _ [] = ""
evaluateVars vt backCount (c:rest)
  | c == '\\' = c : evaluateVars vt (backCount + 1) rest
  | c == '$'  = if isSkipped then 
                  c : evaluateVars vt 0 rest 
                else
                  varVal ++ evaluateVars vt 0 (drop varNameLen rest)
  | otherwise = c : evaluateVars vt 0 rest                
  where isSkipped  = odd backCount
        varName    = extractVarName rest
        varNameLen = length varName
        varVal     = case (M.lookup varName vt) of
                       Just s  -> s
                       Nothing -> ""
        
extractVarName :: String -> String
extractVarName str
  | null str || h == '\\' || isSpace h || h == '$' = ""
  | otherwise = h : (extractVarName $ tail str)
  where h = head str
  
skipChars :: String -> String
skipChars []       = ""
skipChars [c]      = [c]
skipChars (c:rest@(r:est))
  | c == '\\' = r : skipChars est
  | otherwise = c : skipChars rest
  
normalizePaths :: ScriptState -> [String] -> [String]
normalizePaths ss paths = map appendWD paths
  where workingDir = wd ss
        appendWD path = if isRelative path then 
                          workingDir </> path 
                        else 
                          path
                          
--executes a command and returns the new script state, also handles input output redirection
executeCommand :: String -> [String] -> FilePath -> FilePath -> Bool -> ScriptState -> IO ScriptState
executeCommand name args input output append ss = do
  let cmd = M.lookup name command_tb
  if isNothing cmd then do
    let message = "Command " ++ name ++ " not recognized!"
    putStrLn message
    return ss{output=message}
  else do
    let comm    = fromJust cmd
        defIn   = null input
        defOut  = null output
    inHand  <- if null input then return stdin else openFile input ReadMode
    outHand <- if null output then 
                 return stdout 
               else 
                 openFile output (if append then AppendMode else WriteMode)
    nss <- comm args ss inHand outHand
    when (not defIn) $ hClose inHand
    when (not defOut) $ hClose outHand
    return nss
  