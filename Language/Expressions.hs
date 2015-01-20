module Language.Expressions(
  Cmd(..),
  TLExpr(..),
  Comp(..),
  Pred(..),
  Conditional(..),
  Loop(..)
) where

data Cmd = Cmd { name :: String -- The command name (can be a variable)
               , args :: [String] -- The command arguments
               , inDir :: Maybe String -- A redirected input fp
               , outDir :: Maybe String -- A redirected output fp
               , append :: Bool -- If redirected, is it appending?
               }
         | Assign { var :: String -- Assignment target
                  , val :: String -- A value to assign to a variable
                  }
         | Comment String
           deriving Show
           
data Comp = CEQ String String
  | CNE String String
  | CGE String String
  | CGT String String
  | CLE String String
  | CLT String String
  | CLI String
  deriving (Eq, Show)


data Pred = Pred Comp
  | Not Pred
  | And Pred Pred
  | Or Pred Pred
  | Parens Pred
  deriving (Eq, Show)

data Conditional = If { cond :: Pred
    , cthen :: [Cmd]
  }
  | IfElse { cond :: Pred
    , cthen :: [Cmd]
    , celse :: [Cmd]
  } deriving Show
  
data Loop = For { varF :: String
    , vals :: [String]
    , coms :: [Cmd]
  } deriving Show

data TLExpr = TLCmd Cmd | TLConditional Conditional | TLLoop Loop deriving Show