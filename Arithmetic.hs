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

showExp :: Exp -> String
showExp (Add exp1 exp2) = "(" ++ (showExp exp1) ++ " + " ++ (showExp exp2) ++ ")"
showExp (Sub exp1 exp2) = "(" ++ (showExp exp1) ++ " - " ++ (showExp exp2) ++ ")"
showExp (Mul exp1 exp2) = "(" ++ (showExp exp1) ++ " * " ++ (showExp exp2) ++ ")"
showExp (Div exp1 exp2) = "(" ++ (showExp exp1) ++ " / " ++ (showExp exp2) ++ ")"
showExp (Pow exp1 exp2) = "(" ++ (showExp exp1) ++ " ^ " ++ (showExp exp2) ++ ")"
showExp _ = error "Not a valid expression"

evalSimple :: Exp -> Integer
evalSimple (Cst m) = m
evalSimple (Add exp1 exp2) = (evalSimple exp1) + (evalSimple exp2)
evalSimple (Sub exp1 exp2) = (evalSimple exp1) - (evalSimple exp2)
evalSimple (Mul exp1 exp2) = (evalSimple exp1) * (evalSimple exp2)
evalSimple (Div exp1 exp2) = (evalSimple exp1) `div` (evalSimple exp2)
evalSimple (Pow exp1 exp2) = (evalSimple exp1) ^ (evalSimple exp2)
evalSimple _ = error "Not a valid expression"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv name value env = (\_v -> if _v == name then Just value else env _v)

evalFull :: Exp -> Env -> Integer
evalFull (If test yes no) env       = if (evalFull test env) /= 0 
                                      then evalFull yes env 
                                      else evalFull no env
evalFull (Var vName) env            = case (env vName) of
                                      Nothing -> error "Variable does not exist"
                                      Just x  -> x
evalFull (Let vName aux body) env   = let newEnv = extendEnv vName (evalFull aux env) env in
                                        evalFull body newEnv  
evalFull exp _                      = evalSimple exp

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
