
import Expr
import Error
import Eval
import Test.Tasty
import Test.Tasty.HUnit
import StateDemo
import qualified Data.Map.Strict as M
import Control.Exception (assert)



evalTests :: TestTree
evalTests = testGroup "Eval tests"
  [ testCase "Base test1" $ assertEqual "eval on number is number" (execState (eval (Const 777))M.empty) (Right 777)
  , testCase "Base test2" $ assertEqual "" (execState (eval (Binop Pow (Binop Div (Binop Mult (Binop Minus (Binop Pow (Binop Plus (SquareRoot(Const 25)) (Const 3)) (Const 2)) (Binop Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3)) )M.empty) (Right ( ((  ( ( sqrt 25 + 3)**2 - 4 * 7) * 2) / 5)**3))
  , let e1 = Binop Mult (Const 4) (Const 5)
        e2 = Binop Plus (Const 1) (Const (-1)) in
    testCase "Zero division test" $ assertEqual "" (execState (eval (Binop Div e1 e2)) M.empty) ( Left (DivisionByZero e1 e2))
  , let e = Binop Minus (Const 1) (Const 100) in
  
    testCase "RootOfNegative error" $ assertEqual "" (execState (eval (SquareRoot e)) M.empty) (Left $ RootOfNegative e)
  , testCase "Var test" $ assertEqual "" (execState (eval (Var "x")) $ M.fromList [("x", 100)]) (Right 100)

  
  , testCase "Undefined variable" $ assertEqual "" (execState (eval (Binop Plus (Var "z") (Var "y"))) $ M.fromList [("x", 10), ("y", 20)]) ( Left $ UndefinedVariable "z")
  , testCase "Base test 3" $ assertEqual "" (execState (eval (Binop Minus (Var "x") (Var "x"))) $ M.fromList [("x", 0)]) (Right 0)
  , testCase "Pow test 0^0" $ assertEqual "" (execState (eval (Binop Pow (Var "x") (Var "y"))) $ M.fromList [("x", 0), ("y", 0)]) (Right 1)
  ]

exprTests :: TestTree
exprTests = testGroup "Expr tests" [evalTests]

main :: IO()
main = defaultMain exprTests