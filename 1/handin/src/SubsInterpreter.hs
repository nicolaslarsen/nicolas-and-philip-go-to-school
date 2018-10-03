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
                      (env, _) -> Right (x, env)
  m >>= f = SubsM $ \(env, penv) -> case runSubsM m (env,penv) of
                              Right (a,newEnv)  -> runSubsM (f a) (newEnv,penv)
                              Left err          -> Left err
  fail s =  SubsM $ \_-> Left s

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
plus [(IntVal x), (IntVal y)] = Right $ IntVal $ x+y
plus [(StringVal x), (StringVal y)] = Right $ StringVal $ x ++ y
plus [(IntVal x), (StringVal y)] = Right $ StringVal $ show x ++ y
plus [(StringVal x), (IntVal y)] = Right $ StringVal $  x ++ show y
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
modifyEnv f = SubsM $ \(env, _) -> Right ((), f env)

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv $ Map.insert name val

getVar :: Ident -> SubsM Value
getVar name = SubsM $ \(env, _) -> case Map.lookup name env of
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
                    Array exps -> do
                            vals <- let subsList = map evalExpr exps in
                                        concatSubsM subsList
                            return $ ArrayVal vals
                    Var ident -> getVar ident
                    Assign ident exp1 -> do
                            value <- evalExpr exp1
                            putVar ident value
                            getVar ident
                    Call fun exps -> do
                            f <- getFunction fun
                            vals <- let subsMList = map evalExpr exps in
                                        concatSubsM subsMList
                            case f vals of
                              Left err -> fail err
                              Right val -> return val
                    Comma exp1 exp2 -> evalExpr exp1 >> evalExpr exp2
                    Compr (ACBody exp1) -> evalExpr exp1
                    Compr (ACFor ident exp1 arrCmp) -> do
                            list <- evalExpr exp1
                            case list of
                              ArrayVal arr -> do
                                vals <- let subsMList = map mapHelp arr
                                        in concatSubsM subsMList
                                -- The filter is just used to remove undefined values,
                                -- since we return them in the ACIf if the expression
                                -- returns false.
                                return $ ArrayVal (filter (/= UndefinedVal) vals)
                              _ -> fail "Expression must be an ArrayVal"
                      -- binds the id to a variable x then evaluates exp1 with this new context
                      where mapHelp x = putVar ident x >> evalExpr (Compr arrCmp)
                    Compr (ACIf exp1 arrCmp) -> do
                            bool <- evalExpr exp1
                            case bool of
                              TrueVal   -> evalExpr (Compr arrCmp)
                              FalseVal  -> return UndefinedVal
                              _         -> fail "Not a boolean"
                    TrueConst -> return TrueVal
                    FalseConst -> return FalseVal
                    Undefined -> return UndefinedVal

-- "Concatinates" an entire list of SubsM Values into a single SubsM [Value]
concatSubsM :: [SubsM Value] -> SubsM [Value]
concatSubsM = foldr (liftM2 (:)) (return [])

runExpr :: Expr -> Either Error Value
runExpr exp1 = case runSubsM (evalExpr exp1) initialContext of
                              Left er -> Left er
                              Right (a, _) -> Right a
