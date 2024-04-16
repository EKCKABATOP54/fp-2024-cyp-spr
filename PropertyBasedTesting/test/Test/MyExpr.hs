module Test.MyExpr where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Debug.Trace

import Expr.Expr as Expr
import Expr.StateDemo as StateDemo
import Expr.Eval as Eval
import qualified Expr.ExprParser as Parser

genBinop :: Gen BinOptor
genBinop = Gen.element [Plus, Minus, Mult, Div, Pow]

genExpr :: Int -> Int -> Gen (Expr Int)
genExpr n varLength =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      numGen
      , varGen
    ]
    [ -- recursive generators
      binOpGen
      , unOpGen 
    ]
  where
    varGen = Expr.Var <$> Gen.string (Range.constant 1 varLength) Gen.alpha
    numGen = Expr.Const <$> Gen.int (Range.constant 1 n)
    unOpGen = Gen.subterm (genExpr n varLength) (SquareRoot)
    binOpGen = do
      op <- genBinop
      Gen.subterm2 (genExpr n varLength) (genExpr n varLength) (Binop op)
  
genExprDouble :: Int -> Gen (Expr Double)
genExprDouble varLength =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      numGen
      , varGen
    ]
    [ -- recursive generators
      binOpGen
      , unOpGen 
    ]
  where
    varGen = Expr.Var <$> Gen.string (Range.constant 1 varLength) Gen.alpha
    numGen = Expr.Const <$> Gen.double (Range.constant 1.0 10.0)
    unOpGen = Gen.subterm (genExprDouble varLength) (SquareRoot)
    binOpGen = do
      op <- genBinop
      Gen.subterm2 (genExprDouble varLength) (genExprDouble varLength) (Binop op)


-- parser . printer == id
parserPrinterIsId :: MonadTest m => (Expr Int -> String) -> (String -> Maybe (String, Expr Int)) -> Expr Int -> m ()
parserPrinterIsId printer parser ast =
  case parser (printer ast) of
    Just ("", r) -> trace (show ast)(r === ast)
    _ -> trace (show ast) failure

prop_printerParserPrefix :: Property
prop_printerParserPrefix = property $ do
  expr <- forAll $ genExpr 100 1
  parserPrinterIsId printPrefix (Parser.runParser Parser.parseExpr) expr

prop_divisionSelfIsOne :: Property
prop_divisionSelfIsOne = property $ do
  expr <- forAll $ genExprDouble 1
  let result = execState (Eval.eval (Binop Div expr expr)) Eval.emptyBinding
  case result of
    Right val ->
      if val == 1
        then success 
        else failure
    Left err -> success

prop_multOne :: Property
prop_multOne = property $ do
  expr <- forAll $ genExprDouble 100
  let result1 = execState (Eval.eval (Binop Mult expr 1)) Eval.emptyBinding
  let result2 = execState (Eval.eval (Binop Mult 1 expr)) Eval.emptyBinding
  if result1 == result2 && result1 == execState (Eval.eval expr) Eval.emptyBinding then success
  else failure

prop_multZero :: Property
prop_multZero = property $ do
  expr <- forAll $ genExprDouble 100
  let result1 = execState (Eval.eval (Binop Mult expr 0)) Eval.emptyBinding
  let result2 = execState (Eval.eval (Binop Mult 0 expr)) Eval.emptyBinding
  case (result1, result2) of
    (Right v1, Right v2) ->   if (result1 == result2) && (result1 == execState (Eval.eval (Const 0)) Eval.emptyBinding) then success
                              else failure
    _ -> success

props :: [TestTree]
props =
  [
    testProperty "`parser . printer == id` for Prefix" prop_printerParserPrefix
    , testProperty "e/e=1"  prop_divisionSelfIsOne
    , testProperty "e*1 = 1*e = e"  prop_multOne
    , testProperty "e*0 = 0*e = 0"  prop_multZero
  ]
