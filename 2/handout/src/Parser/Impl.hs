module Parser.Impl where

-- Put your parser implementation in this file (and, if appropriate,
-- in other files in the Parser/ subdirectory)

import SubsAst
import Text.ParserCombinators.ReadP as ReadP  
import Control.Applicative as Applicative
-- You can change this if you want, but must be an instance of Show and Eq
data ParseError = ParseError String
                deriving (Show, Eq)

{--



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
  op1 <- ex1 <|> ex2 <|> ex3 <|> ex5
  fn <- funParser
  


--}

valP = trueP <|> falseP <|> undP <|> numP

trueP = do 
  string "true"
  return (TrueConst)

falseP = do 
  string "false"
  return (FalseConst)

undP = do 
  string "undefined"
  return (Undefined) 
{--
ex4 = do 
  str <- munch (\c -> True)
  return (String str)
--}


--symbol = skipSpaces . string


numP = do 
  num <- munch (\char -> char >= '0' && char <= '9')
  return (Number $ read num)

{-- e = do val <- p1
    eopt val --}
token p = skipSpaces >> p
symbol = token . string

e1 = do nm <- valP 
        ev <- eopt nm
        return ev

eopt inval = (do 
                 fnHelper inval "-"
             <|> fnHelper inval "+"
             <|> fnHelper inval "%"
             <|> fnHelper inval "*"
             <|> fnHelper inval "/"
             <|> fnHelper inval "==="
             <|> return inval)

fnHelper inval c = (do symbol c 
                       nm <- valP
                       ev <- eopt (Call c [inval, nm])
                       return ev)

 
