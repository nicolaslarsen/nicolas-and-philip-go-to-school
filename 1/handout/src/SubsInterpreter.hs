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
                       , ("%", undefined)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Monad SubsM where
  return x = SubsM $ \c -> case c of
                      (env, penv) -> Right (x, env)
                      _ -> Left "Err"
  --m >>= f = undefined
  m >>= f = SubsM $ \(c1,c2) -> case m of SubsM x ->
                                              let var = x (c1,c2) in
                                                case var of
                                                  Right (a, env) -> case f a of
                                                                      SubsM y -> y (env,c2)
                                                  Left er -> Left er
                                     --     case var of
                                      --      Right (a, env) -> Right (a,env)
                                       --     Left err -> Left err
  fail s = undefined


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
plus _ = Left "test"

mul :: Primitive
mul [(IntVal x), (IntVal y)] = Right $ IntVal (x*y)
mul _ = Left "test"

sub :: Primitive
sub [(IntVal x), (IntVal y)] = Right $ IntVal (x-y)
sub _ = Left "test"

lol :: Context -> Either Error (Int, Env)
lol (env, penv) = Right (1, env)
lol _ = Left "hej"

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = undefined

putVar :: Ident -> Value -> SubsM ()
putVar name val = undefined

getVar :: Ident -> SubsM Value
getVar name = undefined

getFunction :: FunName -> SubsM Primitive
getFunction name = undefined

evalExpr :: Expr -> SubsM Value
evalExpr expr = undefined

runExpr :: Expr -> Either Error Value
runExpr expr = undefined
