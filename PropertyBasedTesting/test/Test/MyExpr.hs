module Test.MyExpr where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Debug.Trace

import Expr.Expr as Expr
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
    numGen = Expr.Const <$> Gen.int (Range.constant 0 n)
    unOpGen = Gen.subterm (genExpr n varLength) (SquareRoot)
    binOpGen = do
      op <- genBinop
      Gen.subterm2 (genExpr n varLength) (genExpr n varLength) (Binop op)


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

props :: [TestTree]
props =
  [
    testProperty "`parser . printer == id` for Prefix" prop_printerParserPrefix
  ]
