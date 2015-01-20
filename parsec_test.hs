import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, spaces, letter, alphaNum)
import Text.Parsec (parse, ParseError, try)
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), Applicative, (<|>), many)
import Data.Char (digitToInt)
import Text.Parsec.Combinator (many1, sepBy1)
import qualified Data.Map as M
import Control.Monad (join)
import Text.Parsec.Expr

err = "An error has occurred"

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p = parse p err

parseDigit :: Parser Int
parseDigit = digitToInt <$> digit

parsePosInt :: Parser Int
parsePosInt = read <$> many1 digit

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = ((:) <$> a) <*> b

negative :: Parser Int
negative = read <$> (char '-' <:> many1 digit)

integer :: Parser Int
integer = parsePosInt <|> negative

token  :: Parser a -> Parser a
token = (<* spaces)

betterParse :: Parser a -> String -> Either ParseError a
betterParse p = parse (spaces *> p) err

integer' = token integer

symbol :: Char -> Parser Char
symbol = token . char

data MyList = WordList { text :: [String] }
            | IntList  { ints :: [Int] } deriving (Eq, Show)
            
brackets :: Parser a -> Parser a
brackets = (<* symbol ']') . (symbol '[' *>)

elements :: Parser a -> Parser [a]
elements = flip sepBy1 (symbol ',')

listOf :: Parser a -> Parser [a]
listOf = brackets . elements . token

list :: Parser MyList
list = intlist <|> strlist
  where intlist = IntList  <$> listOf integer'
        strlist = WordList <$> listOf (many1 letter)
        
list' :: Parser MyList
list' = try intlist <|> strlist
  where intlist = IntList  <$> listOf integer'
        strlist = WordList <$> listOf (many1 letter)
        
data Expression = Val   Int
                | Var   String
                | Plus  Expression Expression
                | Minus Expression Expression
                  deriving Show
                
data Assignment = Assignment String Expression deriving Show

type Program = [Assignment]

type VarTable = M.Map String Int

eval :: VarTable -> Expression -> Maybe Int
eval vt e = case e of (Val     v) -> Just v
                      (Var     v) -> M.lookup v vt
                      (Plus  a b) -> (+) <$> eval vt a <*> eval vt b
                      (Minus a b) -> (-) <$> eval vt a <*> eval vt b
                      
assign :: Assignment -> VarTable -> Maybe VarTable
assign (Assignment var exp) vt = insert var vt <$> eval vt exp
  where insert k = flip (M.insert k)
  
run :: Program -> Maybe VarTable
run = run' (Just M.empty)
  where run' vt []     = vt
        run' vt (a:as) = run' (join $ assign a <$> vt) as
        
program' = [ Assignment "a" (Plus (Val 7) (Val (-3)))
          , Assignment "b" (Minus (Var "a") (Val 1))
          , Assignment "a" (Val (-2))
          , Assignment "c" (Plus (Var "a") (Var "b"))
          ]
          
variable :: Parser String
variable = token $ letter <:> many alphaNum

table = [[binary '+' Plus, binary '-' Minus]]
  where binary name f = Infix (f <$ (symbol name)) AssocLeft
  
expression :: Parser Expression
expression = buildExpressionParser table other
  where other = val <|> var
        var   = Var <$> variable
        val   = Val <$> integer'
        
assignment :: Parser Assignment
assignment = Assignment <$> variable <*> (symbol '=' *> expression)

program :: Parser Program
program = many1 assignment

interpret :: String -> Maybe VarTable
interpret s = case prog of Left  e -> Nothing
                           Right p -> run p
  where prog = betterParse program s
  
example = "a = 7 - 9 + 3 + 15 b = a - 9 c = a + a + b a = c + b"