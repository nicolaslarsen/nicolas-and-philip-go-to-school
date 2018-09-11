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
showExp (Cst x)
  | x < 0 = "(" ++ show x ++ ")"
  | otherwise = show x 
showExp (Add exp1 exp2) = "(" ++ (showExp exp1) ++ "+" ++ (showExp exp2) ++ ")"
showExp (Sub exp1 exp2) = "(" ++ (showExp exp1) ++ "-" ++ (showExp exp2) ++ ")"
showExp (Mul exp1 exp2) = "(" ++ (showExp exp1) ++ "*" ++ (showExp exp2) ++ ")"
showExp (Div exp1 exp2) = "(" ++ (showExp exp1) ++ "/" ++ (showExp exp2) ++ ")"
showExp (Pow exp1 exp2) = "(" ++ (showExp exp1) ++ "^" ++ (showExp exp2) ++ ")"
showExp _ = error "Not a valid expression"

power x y 
  | y < 0 = error "test"
  | y == 0 = 1
  | y == 1 = x 
  | otherwise = x * power x (y-1)

evalSimple :: Exp -> Integer
evalSimple (Cst m) = m
evalSimple (Add exp1 exp2) = (evalSimple exp1) + (evalSimple exp2)
evalSimple (Sub exp1 exp2) = (evalSimple exp1) - (evalSimple exp2)
evalSimple (Mul exp1 exp2) = (evalSimple exp1) * (evalSimple exp2)
evalSimple (Div exp1 exp2) = (evalSimple exp1) `div` (evalSimple exp2)
evalSimple (Pow exp1 exp2) = let x = (evalSimple exp1) in 
                               power x (evalSimple exp2)
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
evalFull (Sum v from to body) env   = listSum [(evalFull from env)..(evalFull to env)] v env body
  where listSum [] _ _ _ = 0
        listSum (x:xs) vName env body = (evalFull (Let vName (Cst x) body) env) + listSum xs vName env body
evalFull exp _                      = evalSimple exp

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst a) _ = Right a
evalErr (Div exp1 exp2) env = case evalErr exp1 env of 
                                Left er -> Left er
                                Right x -> case evalErr exp2 env of
                                        Left er -> Left er 
                                        Right y -> if y == 0 then Left EDivZero
                                                             else Right (x `div` y)
evalErr (Pow exp1 exp2) env = case evalErr exp1 env of
                                Left er -> Left er
                                Right x -> case evalErr exp2 env of
                                        Left er -> Left er
                                        Right y -> if y == 0 then Left ENegPower else Right (x ^ y)
evalErr (Sub exp1 exp2) env = case evalErr exp1 env of
                                Left er -> Left er
                                Right x -> case evalErr exp2 env of
                                        Left er -> Left er
                                        Right y -> Right (x - y)
evalErr (Mul exp1 exp2) env = case evalErr exp1 env of
                                Left er -> Left er
                                Right x -> case evalErr exp2 env of
                                        Left er -> Left er
                                        Right y -> Right (x * y)
evalErr (Add exp1 exp2) env = case evalErr exp1 env of
                                Left er -> Left er
                                Right x -> case evalErr exp2 env of 
                                        Left er -> Left er
                                        Right y -> Right (x + y)
evalErr (If test yes no) env = case evalErr test env of
                                  Left er -> Left er
                                  Right x -> if x /= 0 
                                                then case evalErr yes env of
                                                        Left er -> Left er
                                                        Right x -> Right x 
                                                else case evalErr no env of
                                                        Left er -> Left er
                                                        Right x -> Right x
evalErr (Var vName) env = case (env vName) of
                            Nothing -> Left (EBadVar vName)
                            Just x  -> Right x
evalErr (Let vName aux body) env = case evalErr aux env of
                                      Left er -> Left er
                                      Right x -> let newEnv = extendEnv vName x env in
                                        case evalErr body newEnv of
                                          Left er -> Left er
                                          Right x -> Right x
evalErr (Sum vName from to body) env = case evalErr from env of 
                                          Left er -> Left er
                                          Right x -> case evalErr to env of 
                                                        Left er -> Left er
                                                        Right y -> errListSum [x..y] vName env body

errListSum [] _ _ _ = Right 0
errListSum (x:xs) vName env body = case (evalErr (Let vName (Cst x) body) env) of 
                                  Left er -> Left er
                                  Right x -> case (Right x, (errListSum xs vName env body)) of
                                              (Right m, Right n)  -> Right (m+n)
                                              (_, Left err) -> Left err
                                              (Left err, _) -> Left err

                               
-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
