module Parser.Impl where

-- Put your parser implementation in this file (and, if appropriate,
-- in other files in the Parser/ subdirectory)

import SubsAst
import Text.ParserCombinators.ReadP
import Control.Applicative

-- You can change this if you want, but must be an instance of Show and Eq
data ParseError = ParseError String
                deriving (Show, Eq)


parseString :: String -> Either ParseError Expr
parseString = undefined

isLetter :: Char -> Bool
isLetter char = (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')

isDigit :: Char -> Bool
isDigit char = char >= '0' && char <= '9'

ident :: ReadP Expr
ident = do
   firstLetter <- satisfy isLetter
   remainder <- many1 (satisfy (\c -> isLetter c || isDigit c))
   satisfy (== ' ')
   return (Var  ([firstLetter] ++  remainder))
   
number1 :: ReadP Expr 
number1 = do 
   firstDigit <- satisfy (\c -> isDigit c &&  c /= '0')
   remainder <- many1 (satisfy isDigit)
   satisfy (== ' ')
   return (Number (read ([firstDigit] ++ remainder)::Int))

number0 :: ReadP Expr
number0 = do
   satisfy (\c -> c == '0')
   satisfy (== ' ')
   return (Number 0) 

number :: ReadP Expr
number = do 
   val <- number0 <|> number1
   return val

string1  word = do
   result <- string word
   return (String result)

trueReader = do
  string "true "
  return TrueConst

falseReader = do
  string "false "
  return FalseConst

undefinedReader = do
  string "undefined "
  return Undefined

arrayFor = do 
   string "for "
   string "("
   iden <- ident
   string " "
   string "of"
   exp1 <- expr1
   string ")"
   ac <- arraycompr
   case iden of
      Var id -> return (ACFor id exp1 ac)
      _ -> return (ACFor "royal fuck up" exp1 ac)

arrayBody = do
   result <- expr1
   return (ACBody result)

arrayIf = do 
   string "if"
   string " "
   string "("
   exp1 <- expr1
   string ")"
   string " "
   ac <- arraycompr
   return (ACIf exp1 ac)


arraycompr = do
   result <- arrayFor <|> arrayIf <|> arrayBody
   return result





isInString char = (char >= '!' && char <= '~' && char /= '\\' && char /= '\'')

stringReader = do
  satisfy (== '\'')
  str <- many1 (satisfy isInString)
  satisfy (== '\'')
  return (String str)
 
expr1 = do 
   val <- trueReader <|> falseReader <|> ident <|> number
   return val
