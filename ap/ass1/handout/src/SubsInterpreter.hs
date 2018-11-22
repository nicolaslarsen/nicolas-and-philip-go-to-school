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

--Mapping the Primitive environments with signs -> function
initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", mkEquality)
                       , ("<", mkLess)
                       , ("+", mkSum)
                       , ("*", mkMul)
                       , ("-", mkSub)
                       , ("%", mkDiv)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Monad SubsM where
  return x = SubsM(\c@(e,pe) -> Right (x,e))
  -- m >>= f = f m
  m >>= f = SubsM(\s0@(env,penv) -> case runSubsM m s0 of
    (Left err) -> (Left err)
    (Right (a, s1)) ->  runSubsM (f a) (s1, penv))
  fail s = SubsM(\e -> Left s)

-- You may modify these if you want, but it shouldn't be necessary
instance Functor SubsM where
  fmap = liftM
instance Applicative SubsM where
  pure = return
  (<*>) = ap

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

mkEquality :: Primitive
mkEquality ((IntVal x):(IntVal y):[]) = if (x==y) then return TrueVal else return FalseVal 
mkEquality ((TrueVal):(TrueVal):[]) = return TrueVal
mkEquality _ = Left "=== called with wrong number or type of arguments"

mkLess :: Primitive
mkLess ((IntVal x):(IntVal y):[]) = if (x<y) then return TrueVal else return FalseVal
mkLess _ = Left "< called with wrong number or type of arguments"

mkSum :: Primitive
mkSum ((IntVal x):(IntVal y):[]) = return (IntVal (x+y))
mkSum _ = Left "+ called with wrong number or type of arguments"

mkMul :: Primitive
mkMul ((IntVal x):(IntVal y):[]) = return (IntVal (x*y))
mkMul _ = Left "* called with wrong number or type of arguments"

mkSub :: Primitive
mkSub ((IntVal x):(IntVal y):[]) = return (IntVal (x-y))
mkSub _ = Left "- called with wrong number or type of arguments"

mkDiv :: Primitive
mkDiv ((IntVal x):(IntVal y):[]) = return (IntVal (x `div` y))
mkDiv _ = Left "% called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM (\(env,penv) -> Right( (), f env))

modifyEnvH :: Env -> Env 
modifyEnvH env = do
  let (envN,penv) = initialContext
  Map.union env envN

putVar :: Ident -> Value -> SubsM ()
putVar name val = SubsM (\(env,penv) -> Right((), (Map.insert name val env)))

getVar :: Ident -> SubsM Value
getVar name = SubsM (\(env,penv) -> 
  do
    let result = Map.lookup name env
    case result of
      Nothing -> Left "No variable found"
      (Just f) -> Right (f,env))

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM (\(env,penv) -> 
  do
    let result = Map.lookup name penv
    case result of 
      Nothing -> Left "No function found"
      (Just f) -> Right (f,env))

evalExpr :: Expr -> SubsM Value
evalExpr (Number num) = SubsM (\(env,penv) -> return ((IntVal num), env))
evalExpr (String str) = SubsM (\(env,penv) -> return ((StringVal str), env))
evalExpr (TrueConst) = SubsM (\(env,penv) -> return ((TrueVal),env))
evalExpr (FalseConst) = SubsM (\(env,penv) -> return ((FalseVal),env))
evalExpr (Var ident) = getVar ident
-- evalExpr (Assign ident expr) =  ((TrueVal), putVar ident (evalExpr expr)))
--evalExpr (Assign ident expr) = SubsM (\(env,penv) -> 
 {- o
    res <- evalExpr expr
  -- let (_,env) = 
    return (TrueVal,(putVar ident res )))
evalExpr (Undefined) = SubsM(\(env,penv) -> return ((UndefinedVal),env))
evalExpr (Call op (expr0 : expr1 : rest)) = SubsM (\(env,penv) -> 
   if rest == [] then
    do
     (res1, _) <- runSubsM (evalExpr expr0) (env,penv)
     (res2, _) <-  runSubsM (evalExpr expr1) (env,penv)
     (primitive, _) <- runSubsM (getFunction op) (env,penv)
     res <- primitive [res1, res2]
     return (res, env)
     else
      Left ("Incorrect num of arguments"))
evalExpr (Call op _) = SubsM (\(env,penv) -> Left ("Incorrect num of args")) }
-}
evalExpr (Array (expr:rest)) = SubsM(\(env,penv) ->
  do
    (res1,_) <- runSubsM (evalExpr expr) (env,penv)
    -- (ArrayVal res2,_) <- runSubsM (evalExpr expr) (env,penv)
    (res2,_) <- runSubsM (evalExpr expr) (env,penv)
    return (ArrayVal(res1 : [res2]),env))
evalExpr (Comma expr1 expr2) = SubsM(\(env,penv) ->
  do
    (res1,nenv) <- runSubsM (evalExpr expr1) (env,penv)
    (res2,nenv2) <- runSubsM (evalExpr expr2) (nenv,penv)
    return (res2, nenv2))
evalExpr (Compr ac) = undefined

arrayComp :: ArrayCompr -> SubsM Value
-- arrayComp (ACBody expr) = SubsM (\(env,penv) -> runSubsM (evalExpr expr) (env,penv))
-- arrayComp (ACIf expr ac) = SubsM (\(env,penv) -> 
--   do
--     (res,_) <- runSubsM (evalExpr expr) (env,penv)
--     if (res == TrueVal) then return ((runSubsM (arrayComp ac) (env,penv)),env) else return (UndefinedVal,env))
arrayComp (ACFor ident expr ac) = undefined

runExpr :: Expr -> Either Error Value
-- runExpr expr = do
--   let (b, (a,env)) = evalExpr expr
runExpr expr = do
  (v,env) <- runSubsM(evalExpr expr) (initialContext)
  return v
