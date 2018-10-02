module SubsInterpreter
       (
         Value(..)
       , runExpr
       -- You may include additional exports here, if you want to
       -- write unit tests for them.
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", equals)
                       , ("<", lt)
                       , ("+", plus)
                       , ("*", mul)
                       , ("-", sub)
                       , ("%", modulo)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Monad SubsM where
  return x = SubsM $ \c -> case c of
                      (env, penv) -> Right (x, env)
                      _ -> Left "Err"
  m >>= f = SubsM $ \(env, penv) -> case runSubsM m (env,penv) of 
                              Right (a,newEnv)  -> runSubsM (f a) (newEnv,penv)
                              Left err          -> Left err
  fail s =  SubsM $ \context -> Left s
          {--
           SubsM $ \(c1,c2) -> case m of SubsM x ->
                                              let var = x (c1,c2) in
                                                case var of
                                                  Right (a, env) -> case f a of
                                                                      SubsM y -> y (env,c2)
                                                  Left er -> Left er
          --}

-- You may modify these if you want, but it shouldn't be necessary
instance Functor SubsM where
  fmap = liftM
instance Applicative SubsM where
  pure = return
  (<*>) = ap

equals :: Primitive
equals [x, y]
  | x == y = Right TrueVal
  | otherwise = Right FalseVal
equals _ = Left "Values can not be compared"

lt :: Primitive
lt [IntVal x, IntVal y] = if x < y then Right TrueVal else Right FalseVal
lt [StringVal x, StringVal y] = if x < y then Right TrueVal else Right FalseVal
lt _ = Left "Values can not be compared"

plus :: Primitive
plus [(IntVal x), (IntVal y)] = Right $ IntVal (x+y)
plus [(StringVal x), (StringVal y)] = Right $ StringVal (x ++ y)
plus [(IntVal x), (StringVal y)] = Right $ StringVal $ (show x) ++ y
plus [(StringVal x), (IntVal y)] = Right $ StringVal $  x ++ (show y)
plus _ = Left "Values can not be added"

mul :: Primitive
mul [(IntVal x), (IntVal y)] = Right $ IntVal (x*y)
mul _ = Left "Values can not be multiplied"

sub :: Primitive
sub [(IntVal x), (IntVal y)] = Right $ IntVal (x-y)
sub _ = Left "Values can not be subtracted"

modulo :: Primitive
modulo [(IntVal x), (IntVal y)] = Right $ IntVal (x `mod` y)
modulo _ = Left "Values can not be modded"

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM $ \(env, penv) -> Right ((), f env)

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv $ Map.insert name val

getVar :: Ident -> SubsM Value
getVar name = SubsM $ \(env, penv) -> case Map.lookup name env of
                                        Just a  -> Right (a, env)
                                        Nothing -> Left "Error"

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM $ \(env, penv) -> case Map.lookup name penv of
                                        Just a  -> Right (a, env)
                                        Nothing -> Left "Error"

evalExpr :: Expr -> SubsM Value
evalExpr expr = case expr of
                    Number i -> return $ IntVal i
                    String s -> return $ StringVal s
                    Array [] -> return $ ArrayVal []
                    Array arr -> return $ helpEval (Array arr)
                    Var id -> getVar id
                    Assign id exp -> do 
                            value <- evalExpr exp
                            putVar id value
                            getVar id
                            --(evalExpr exp) >>= putVar id >> getVar id
                    Call fun exps@(x:xs) -> return ( case runSubsM (getFunction fun) initialContext of
                                              Right (prim, _) -> case prim (exprToValueList exps) of 
                                                                Right a -> a
                                                                Left err -> UndefinedVal
                                              Left _ -> UndefinedVal)
                            --f <- (getFunction fun)
                            --SubsM $ f (exprToValueList exps)
                            --return $ (getFunction fun initialContext) (exprToValueList exps)
                    Comma exp1 exp2 -> evalExpr exp1 >> evalExpr exp2
                    TrueConst -> return TrueVal
                    FalseConst -> return FalseVal
                    Undefined -> return UndefinedVal
                    _ -> return UndefinedVal

helpEval :: Expr -> Value
helpEval (Number i)   = IntVal i
helpEval (String s)   = StringVal s
helpEval (Array arr)  = ArrayVal (exprToValueList arr)
helpEval TrueConst    = TrueVal
helpEval FalseConst   = FalseVal
helpEval _    = UndefinedVal

exprToValueList :: [Expr] -> [Value]
exprToValueList list = map helpEval list

runExpr :: Expr -> Either Error Value
runExpr expr = case evalExpr expr of SubsM a -> case a initialContext of
                                                              Left er -> Left er
                                                              Right (a,_) -> Right a
