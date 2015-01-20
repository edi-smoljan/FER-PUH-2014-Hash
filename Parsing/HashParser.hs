module Parsing.HashParser (
  simpleParse,
  readCommandLine,
  readCommandBlock,
  assignment,
  command, 
  comment,
  tlexpressions
) where
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, spaces, space, letter, alphaNum, anyChar, satisfy, noneOf, string)
import Text.Parsec (parse, ParseError, try)
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), Applicative, (<|>), many)
import Data.Char (digitToInt, isSpace, isAlphaNum)
import Text.Parsec.Combinator (many1, sepBy1, sepEndBy1, count, sepEndBy, choice, notFollowedBy)
import Language.Expressions
import qualified Data.List as L
import Text.Parsec.Expr

err = "An error has occurred"

keywords = ["if", "then", "else", "fi", "do", "done", "for"]
createKeywordParser word = try (notFollowedBy (string word))

keywordAlert :: Parser a -> Parser a
keywordAlert = foldr (\str acc -> (createKeywordParser str *>) . acc) id keywords

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p = parse (spaces *> p) err

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = ((:) <$> a) <*> b

infixr 4 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a] 
a <++> b = ((++) <$> a) <*> b

backslash :: Parser Char
backslash = char '\\'

skippedChar :: Parser String
skippedChar = backslash <:> count 1 anyChar

--dodati mogućnost spacea u quoteovima
multipleCharSkipped :: Parser String
multipleCharSkipped = concat <$> many1 skippedChar

unskippedSingleQuotedString :: Parser String
unskippedSingleQuotedString = many1 $ satisfy unskipped
  where unskipped c = c `notElem` ['\\', '\'']
  
unskippedDoubleQuotedString :: Parser String
unskippedDoubleQuotedString = many1 $ satisfy unskipped
  where unskipped c = c `notElem` ['\\', '\"']
  
unskippedUnquotedString :: Parser String
unskippedUnquotedString = many1 $ satisfy unskipped
  where unskipped c = not $ isSpace c || c `elem` ['\\', '\'', '\"', ';']
  
stringWithSkipped :: Parser String
stringWithSkipped =  concat <$> (many1 $ multipleCharSkipped <|> unskippedUnquotedString)

notKeywordString :: Parser String
notKeywordString = keywordAlert stringWithSkipped

doubleQuotedStringWithSkipped :: Parser String
doubleQuotedStringWithSkipped = doubleQuotes <++> skippedQuotedString <++> doubleQuotes
  where doubleQuotes = count 1 $ char '\"'
        skippedQuotedString = concat <$> (many1 $ multipleCharSkipped <|> unskippedDoubleQuotedString)
  
singleQuotedStringWithSkipped :: Parser String
singleQuotedStringWithSkipped = singleQuotes <++> skippedQuotedString <++> singleQuotes
  where singleQuotes = count 1 $ char '\''
        skippedQuotedString = concat <$> (many1 $ multipleCharSkipped <|> unskippedSingleQuotedString)
  
token :: Parser a -> Parser a
token = (<* spaces)

commandArg :: Parser String
commandArg = doubleQuotedStringWithSkipped <|> singleQuotedStringWithSkipped <|> try notKeywordString

command :: Parser Cmd
command = readCommandFrom <$> (sepEndBy1 commandArg notNewLineSpaces)

notNewLineSpaces = satisfy (\c -> isSpace c && c /= '\n')

readCommandFrom :: [String] -> Cmd
readCommandFrom args
  | null redirIndVal = Cmd name (drop 1 args) Nothing Nothing False
  | otherwise        = Cmd name (drop 1 . take argsToTake $ args) input output append
  where name         = head args
        redirectInd  = L.findIndices (\a -> a `elem` [">>", ">", "<"]) args
        redirectLen  = length redirectInd
        argLen       = length args
        validInd     = reverse [argLen - 2 - i * 2| i <- [0..redirectLen-1]]
        redirIndVal  = validInd `L.intersect` redirectInd
        readIndVal   = (L.findIndices (\a -> a == "<") args) `L.intersect` validInd
        writeIndVal  = (L.findIndices (\a -> a == ">") args) `L.intersect` validInd
        appenIndVal  = (L.findIndices (\a -> a == ">>") args) `L.intersect` validInd
        maxWrite     = max (if null writeIndVal then (-1) else last writeIndVal) (if null appenIndVal then (-1) else last appenIndVal)
        maxRead      = if null readIndVal then (-1) else last readIndVal
        append       = if null appenIndVal then False else (last appenIndVal) == maxWrite
        argsToTake   = minimum . filter (/=(-1)) $ [maxRead, maxWrite]
        input        = if null readIndVal then 
                         Nothing
                       else
                         Just $ args !! (maxRead + 1)
        output       = if null writeIndVal && null appenIndVal then
                         Nothing
                       else
                         Just $ args !! (maxWrite + 1)
                          
variableName :: Parser String
variableName = (letter <|> char '_') <:> (many $ alphaNum <|> char '_')
  where allowed c = isAlphaNum c || c == '_'
  
assignment :: Parser Cmd
assignment = Assign <$> variableName <*> (char '=' *> commandArg)

comment :: Parser Cmd
comment = token (Comment <$> (char '#' <:> (many $ noneOf ['\n'])))

semiColon = token $ char ';'
   
commandSemi :: Parser Cmd
commandSemi = command <* semiColon
       
assignmentSemi :: Parser Cmd
assignmentSemi = assignment <* semiColon

{-commentSemi :: Parser Cmd
commentSemi = comment <* skip
  where skip = token $ char '\n'
  -}
  
readCommandSemi :: Parser Cmd
readCommandSemi = try comment <|> try assignmentSemi <|> try commandSemi

readCommandBlock :: Parser [Cmd]
readCommandBlock = many readCommandSemi

readCommandLine :: Parser Cmd
readCommandLine = try comment <|> try assignment <|> command


tuple :: a -> b -> (a, b)
tuple a b = (a, b)


symbolBetweenTwoArgs :: String -> Parser (String, String)
symbolBetweenTwoArgs symb = tuple <$> argum <*> (operator *> argum)
  where operator    = token $ string symb
        variable    = char '$' <:> variableName
        usualString = many1 alphaNum
        argum       = token $ doubleQuotedStringWithSkipped <|> singleQuotedStringWithSkipped <|> variable <|> usualString
 

comp :: Parser Comp
comp = try ceq <|> try cne <|> try cge <|> try cgt <|> try cle <|> try clt <|> try cli
  where ceq = (uncurry CEQ) <$> symbolBetweenTwoArgs "=="
        cne = (uncurry CNE) <$> symbolBetweenTwoArgs "/="
        cge = (uncurry CGE) <$> symbolBetweenTwoArgs ">="
        cgt = (uncurry CGT) <$> symbolBetweenTwoArgs ">"
        cle = (uncurry CLE) <$> symbolBetweenTwoArgs "<="
        clt = (uncurry CLT) <$> symbolBetweenTwoArgs "<"
        cli = CLI <$> (doubleQuotedStringWithSkipped <|> singleQuotedStringWithSkipped <|> variable)
        variable = char '$' <:> (many1 alphaNum)

table = [[prefix "!" Not], [binary "&&" And, binary "||" Or]]
  where binary name f = Infix (f <$ (token (string name))) AssocLeft
        prefix name f = Prefix (f <$ (token (string name)))
  
pred' :: Parser Pred
pred' = buildExpressionParser table (token other)
  where other = (Pred <$> comp)  <|> (Parens <$> (parens pred'))
        parens = (<* token (string ")")) . (token (string "(") *>)

conditional :: Parser Conditional
conditional = try ifElseExpr <|> ifExpr
  where ifElseExpr = IfElse <$> (token (string "if") *> token pred') 
                       <*> (token (string "then") *> token readCommandBlock) 
                       <*> (token (string "else") *> token readCommandBlock <* token (string "fi"))
        ifExpr     = If <$> (token (string "if") *> pred') 
                       <*> (token (string "then") *> token readCommandBlock <* token (string "fi"))
        
loop :: Parser Loop
loop = For <$> (token (string "for") *> token commandArg) 
         <*> (token (string "in") *> token (sepEndBy1 commandArg notNewLineSpaces))
         <*> (token (string "do") *> token readCommandBlock <* token (string "done"))

tlexpression :: Parser TLExpr
tlexpression = try (TLLoop <$> (token loop)) <|> try (TLConditional <$> (token conditional)) 
                 <|> (TLCmd <$> (token readCommandSemi))

tlexpressions :: Parser [TLExpr]
tlexpressions = many tlexpression

{-
readScript :: Parser a -> Parser [a]
readScript p = sepBy1 pEndSemi spaces 
  where mSpaces   = many $ space
        semiColon = mSpaces <++> char ';' <:> mSpaces
        pEndSemi  = p <* semiColon
        
readScriptContent :: Parser [TLExpr]
readScriptContent = readScript commandParser
  where commandParser = TLCmd <$> (try assignment <|> command)
-}