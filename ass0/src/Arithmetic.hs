-- This is a skeleton file for you to edit

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions
import Data.Maybe

-- Takes an expression and pretty-prints it.
showExp :: Exp -> String
showExp (Cst x)
  | x < 0 = "(" ++ show x ++ ")"
  | otherwise = show x
showExp (Add exp1 exp2) = "(" ++ showExp exp1 ++ "+" ++ showExp exp2 ++ ")"
showExp (Sub exp1 exp2) = "(" ++ showExp exp1 ++ "-" ++ showExp exp2 ++ ")"
showExp (Mul exp1 exp2) = "(" ++ showExp exp1 ++ "*" ++ showExp exp2 ++ ")"
showExp (Div exp1 exp2) = "(" ++ showExp exp1 ++ "/" ++ showExp exp2 ++ ")"
showExp (Pow exp1 exp2) = "(" ++ showExp exp1 ++ "^" ++ showExp exp2 ++ ")"
showExp _ = error "Not a valid expression"

-- Takes an expression and returns its result as an integer
evalSimple :: Exp -> Integer
evalSimple (Cst m) = m
evalSimple (Add exp1 exp2) = evalSimple exp1 + evalSimple exp2
evalSimple (Sub exp1 exp2) = evalSimple exp1 - evalSimple exp2
evalSimple (Mul exp1 exp2) = evalSimple exp1 * evalSimple exp2
evalSimple (Div exp1 exp2) = evalSimple exp1 `div` evalSimple exp2
evalSimple (Pow exp1 exp2) = seq base (base ^ evalSimple exp2) -- force base to be evaluated
  where base = evalSimple exp1
evalSimple _ = error "Not a valid simple expression"

-- Takes a variable name, an integer value to bind it to and an environment,
-- then binds the name to that value and
-- returns an environment with the updated binding
extendEnv :: VName -> Integer -> Env -> Env
extendEnv name value env _v = if _v == name then Just value else env _v

-- Takes an expression and an environment, then evaluates the expression
-- using the variables bound to the environment (if any)
evalFull :: Exp -> Env -> Integer
evalFull (If test yes no) env       = if evalFull test env /= 0
                                        then evalFull yes env
                                        else evalFull no env
evalFull (Var vName) env            = fromMaybe
                                        (error "Variable does not exist") (env vName)
evalFull (Let vName aux body) env   = let newEnv =
                                            extendEnv vName (evalFull aux env) env in
                                            evalFull body newEnv
evalFull (Sum v from to body) env   =
        listSum [(evalFull from env)..(evalFull to env)] v env body
  where listSum [] _ _ _ = 0
        listSum (x:xs) vName env body = evalFull (Let vName (Cst x) body) env
                                      + listSum xs vName env body
-- We have to parse the environment along,
-- so the evalSimple function is insufficient
evalFull (Cst m) _ = m
evalFull (Add exp1 exp2) env = evalFull exp1 env + evalFull exp2 env
evalFull (Sub exp1 exp2) env = evalFull exp1 env - evalFull exp2 env
evalFull (Mul exp1 exp2) env = evalFull exp1 env * evalFull exp2 env
evalFull (Div exp1 exp2) env = evalFull exp1 env `div` evalFull exp2 env
evalFull (Pow exp1 exp2) env = seq base (base ^ evalFull exp2 env) -- force base evaluation
  where base = evalFull exp1 env

--  Takes an expression and an environment.
--  Returns an integer if everything goes well, and an ArithError
--  if an error is found along the way.
evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst a) _ = Right a
evalErr (Add exp1 exp2) env = evalErrHelper exp1 exp2 env (\x y -> Right(x + y))
evalErr (Sub exp1 exp2) env = evalErrHelper exp1 exp2 env (\x y -> Right(x - y))
evalErr (Mul exp1 exp2) env = evalErrHelper exp1 exp2 env (\x y -> Right(x * y))
evalErr (Div exp1 exp2) env = evalErrHelper exp1 exp2 env divide
evalErr (Pow exp1 exp2) env = evalErrHelper exp1 exp2 env power
evalErr (If test yes no) env = case evalErr test env of
                                  Left er -> Left er
                                  Right x -> if x /= 0
                                                then case evalErr yes env of
                                                        Left er -> Left er
                                                        Right x -> Right x
                                                else case evalErr no env of
                                                        Left er -> Left er
                                                        Right x -> Right x
evalErr (Var vName) env = case env vName of
                            Nothing -> Left (EBadVar vName)
                            Just x  -> Right x
evalErr (Let vName aux body) env = case evalErr aux env of
                                      Left er -> Left er
                                      Right x -> let newEnv = extendEnv vName x env in
                                          case evalErr body newEnv of
                                            Left er -> Left er
                                            Right x -> Right x
evalErr (Sum vName from to body) env =
        case evalErr from env of
          Left er -> Left er
          Right x -> case evalErr to env of
                        Left er -> Left er
                        -- We create a list of integers from x to y and pass it
                        Right y -> evalErrSum [x..y] vName env body
  -- errListSum takes a list of integers, a variable name, an environment and a body.
  -- Each integer is bound to the variable name vName to be used in the body.
  -- The body can also use the variables already defined in the given environment
  -- Returns either an ArithErr or the sum of each iteration of the body.
  where evalErrSum [] _ _ _ = Right 0
        evalErrSum (x:xs) vName env body =
            case evalErr (Let vName (Cst x) body) env of
              Left er -> Left er
              Right x -> case evalErrSum xs vName env body of
                            Left er -> Left er
                            Right y -> Right (x + y)

-- Takes two expressions, an environment and a function
-- f (integer -> integer -> integer). Then it checks for errors,
-- if it finds any, it is returned, otherwise f x y is returned.
evalErrHelper exp1 exp2 env f = case evalErr exp1 env of
  Left er -> Left er
  Right x ->
    case evalErr exp2 env of
        Left er -> Left er
        Right y -> f x y

-- Simple functions to return error or integer for a function
divide x y = if y == 0 then Left EDivZero else Right(x `div` y)
power x y = if y < 0 then Left ENegPower else Right (x^y)

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
