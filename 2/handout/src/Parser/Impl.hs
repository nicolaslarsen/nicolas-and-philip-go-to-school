module Parser.Impl where

-- Put your parser implementation in this file (and, if appropriate,
-- in other files in the Parser/ subdirectory)

import SubsAst
import Text.ParserCombinators.ReadP as ReadP  
import Control.Applicative as Applicative
-- You can change this if you want, but must be an instance of Show and Eq
data ParseError = ParseError String
                deriving (Show, Eq)

parseString :: String -> Either ParseError Expr
parseString = undefined


{--
expr = do
  exp <- comma <|> expr1
  return exp

comma = do 
  exp1 <- expr
  c <- satisfy (\c -> c == ',')
  exp2 <- expr
  return (Comma exp1 exp2)
 --}


expr = do 
  exp1 <- expr1
  peak <- look 
  if peak == "" then return exp1 else 
                do exp2 <- comma 
                   return (Comma exp1 exp2)


comma = do 
  char ','
  exp <- expr
  return exp

expr1Paren = do
  char '('
  exp <- expr
  char ')'
  return exp

expr1 = expr1Parser

expr1Parser = do 
  expr <- ex1 <|> ex2 <|> ex3 <|> ex5
  return (expr)


ex1 = do 
  string "true"
  return (TrueConst)

ex2 = do 
  string "false"
  return (FalseConst)

ex3 = do 
  string "undefined"
  return (Undefined) 
{--
ex4 = do 
  str <- munch (\c -> True)
  return (String str)
--}
ex5 = do 
  num <- munch (\char -> char >= '0' && char <= '9')
  return (Number $ read num)


