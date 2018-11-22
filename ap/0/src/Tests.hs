import Definitions
import Arithmetic

showExpTests = [
          showExp (Cst (-1)) == "(-1)",
          showExp (Mul (Add (Cst 1) (Cst (-2))) (Cst 2)) == "((1+(-2))*2)",
          showExp (Pow (Div (Cst 4) (Cst (2))) (Cst 3)) == "((4/2)^3)"
        ]

testShowExp :: Bool
testShowExp = testStuff showExpTests

evalSimpleTests = [
                    evalSimple (Div (Add (Cst 3) (Cst 2)) (Cst 2)) == 2,
                    evalSimple (Pow (Cst 2) (Mul (Cst 2) (Cst 3))) == 64,
                    evalSimple (Sub (Cst 4) (Cst 5)) == (-1)
                  ]

testEvalSimple :: Bool
testEvalSimple = testStuff evalSimpleTests

evalFullTests = let env = extendEnv "y" 37 (extendEnv "x" 0 initEnv) in
                [
                  evalFull (If (Var "x") (Cst 1) (Cst 42)) env == 42,
                  evalFull (Let "z" (Cst 69) (If (Var "y") (Var "z") (Cst 42))) env == 69,
                  evalFull (Sum "v" (Cst 1) (Cst 10) (Pow (Var "v") (Cst 2))) env == 385
                ]

testEvalFull :: Bool
testEvalFull = testStuff evalFullTests

evalErrTests = let env = extendEnv "y" 37 (extendEnv "x" 0 initEnv) in
                [
                  evalErr (If (Var "x") (Cst 1) (Cst 42)) env == Right 42,
                  evalErr (Let "z" (Cst 69) (If (Var "y") (Var "z") (Cst 42))) env == Right 69,
                  evalErr (Sum "v" (Cst 1) (Cst 10) (Pow (Var "v") (Cst 2))) env == Right 385
                ]


testEvalErr :: Bool
testEvalErr = testStuff evalErrTests

testAll :: Bool
testAll = testStuff [
                      testShowExp, 
                      testEvalSimple, 
                      testEvalFull, 
                      testEvalErr
                    ]

testStuff :: [Bool] -> Bool
testStuff [] = True
testStuff (x:xs) = if x == False then x else testStuff xs
