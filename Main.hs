module Main (
  main
) where

import Hash
import System.Environment
main = do
  args <- getArgs
  if null args then
    runInteractive
  else
    runScript $ head args