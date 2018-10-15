import Test.Tasty
import Test.Tasty.HUnit

import SubsInterpreter
import SubsAst

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ 
  testCase "Boolean Tests" $ 
    [runExpr TrueConst, runExpr FalseConst] @?= [Right TrueVal, Right FalseVal],
  testCase "Number Tests" $ 
    runExpr (Number 37) @?= Right (IntVal 37),
  testCase "String Tests" $ 
    runExpr (String "lol") @?= Right (StringVal "lol"),
  testCase "Undefined Tests" $
    runExpr Undefined @?= Right UndefinedVal,
  testCase "Array Tests" $
    runExpr arrExpr @?= Right assResult,
  testCase "Comma Tests" $
    runExpr commaExpr @?= Right commaResult,
  testCase "Functions calls" $ 
    funExpressions @?= funResults,
  {--testCase "Left Identity" $ 
    return a  >>= f @?= f a,
  testCase "Right Identity" $
    return m >>= return = m,
  testCase "Associativity"
    (m >>= f) >>= g @?= m >>= (\x -> f x >>= g)
  --}
  testCase "Other Tests" $ "" @?= ""]
arrExpr = Array [Number 0, Number 1, Number 2, Number 3]
assResult = ArrayVal [IntVal 0, IntVal 1, IntVal 2, IntVal 3]

funExpressions = [runExpr (Call "+" [Number 1, Number 2]), runExpr (Call "/" [Number 6, Number 2])]
funResults = [Right (IntVal 3), Left "Error"] 


introExpr :: Expr
introExpr =
  Comma (Assign "xs"
          (Array [Number 0, Number 1, Number 2, Number 3, Number 4,
                  Number 5, Number 6, Number 7, Number 8, Number 9]))
   (Comma (Assign "squares"
            (Compr (ACFor "x" (Var "xs")
                     (ACBody (Call "*" [Var "x",Var "x"])))))
     (Comma (Assign "evens"
              (Compr (ACFor "x" (Var "xs")
                       (ACIf (Call "===" [Call "%" [Var "x", Number 2],
                                          Number 0])
                         (ACBody (Var "x"))))))
       (Comma (Assign "many_a"
                (Compr (ACFor "x" (Var "xs")
                         (ACFor "y" (Var "xs")
                           (ACBody (String "a"))))))
         (Comma (Assign "hundred"
                  (Compr (ACFor "i" (Array [Number 0])
                           (ACFor "x" (Call "Array" [Number 5])
                             (ACFor "y" (Call "Array" [Number 20])
                               (ACBody (Assign "i"
                                         (Call "+" [Var "i", Number 1]))))))))
           (Array [Var "xs", Var "squares", Var "evens",
                   Var "many_a", Var "hundred"])))))



introResult :: Value
introResult = ArrayVal
  [ ArrayVal [IntVal n | n <- [0..9]]
  , undefined
  , undefined
  , undefined
  , undefined
  ]

scopeExpr :: Expr
scopeExpr =
  Comma (Assign "x" (Number 42))
   (Comma (Assign "y" (Compr (ACFor "x" (String "abc")
                               (ACBody (Var "x")))))
     (Array [Var "x", Var "y"]))

scopeResult :: Value
scopeResult = ArrayVal
  undefined


commaExpr = Comma (Number 82) (String "weTesting")
commaResult = StringVal "weTesting"





