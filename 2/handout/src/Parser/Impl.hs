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

ident :: ReadP String
ident = do
   firstLetter <- satisfy isLetter
   remainder <- many1 (satisfy (\c -> isLetter c || isDigit c))
   satisfy (== ' ')
   return ([firstLetter] ++  remainder)
   
number1 :: ReadP Int 
number1 = do 
   firstDigit <- satisfy (\c -> isDigit c &&  c /= '0')
   remainder <- many1 (satisfy isDigit)
   satisfy (== ' ')
   return (read ([firstDigit] ++ remainder)::Int)

number0 :: ReadP Int
number0 = do
   satisfy (\c -> c == '0')
   satisfy (== ' ')
   return 0 

number :: ReadP Int
number = do 
   val <- number0 <|> number1
   return val
