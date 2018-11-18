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

isLetter char = (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')

isDigit char = char >= '0' && char <= '9'

ident = do
    val <- ident1 <|> ident2
    return val

ident1 = do
    firstLetter <- satisfy isLetter
    remainder <- many1 (satisfy (\c -> isLetter c || isDigit c))
    return (Var  ([firstLetter] ++  remainder))

ident2 = do
  letter <- satisfy isLetter
  return (Var [letter])

number1 = do
   firstDigit <- satisfy (\c -> isDigit c &&  c /= '0')
   remainder <- many1 (satisfy isDigit)
   return (Number (read ([firstDigit] ++ remainder)::Int))

number2 = do
   dig <- satisfy isDigit
   return (Number (read [dig]::Int))


number = do
   val <- number1 <|> number2
   return val

string1  word = do
   result <- string word
   return (String result)

trueReader = do
  string "true"
  return TrueConst

falseReader = do
  string "false"
  return FalseConst

undefinedReader = do
  string "undefined"
  return Undefined

arrayFor = do
   string "for"
   string "("
   iden <- ident
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
   string "("
   exp1 <- expr1
   string ")"
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

commaExpr = do
        exp1 <- expr1
        string ","
        exp2 <- expr1
        return (Comma exp1 exp2)

funReader = do
    exp1 <- expr1
    fun  <- string "+" <|> string "-" <|> string "*" <|> string "%"
            <|> string "<" <|> string "==="
    exp2 <- expr1
    return (Call fun [exp1, exp2])

data Ex =
        Expr
      | Eof
      deriving (Read, Show)

eo = do
    eof
    return Eof

exprs = do
    eo <|> exprs2

exprs2 = do
    exp1  <- expr1
    ce    <- commaExprs
    case ce of
      Eof  -> return exp1
      _   -> return (Comma exp1, ce)

expr = do
    val <- expr1 <|> commaExpr
    return val

expr1 = do
   val <- trueReader <|> falseReader <|> ident <|> number <|>
           undefinedReader
   skipSpaces
   return val
