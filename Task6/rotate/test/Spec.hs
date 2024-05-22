
import Rotate

import Test.Tasty
import Test.Tasty.HUnit


finite_tests :: TestTree
finite_tests = testGroup "finite tests"
  [ testCase "Base test1" $ assertEqual "Base test1" "cdefab" (rotate 2 "abcdef"),
    testCase "Base test2" $ assertEqual "Base test2" "efabcd" (rotate (-2) "abcdef")
  ]




rotateTestts :: TestTree
rotateTestts = testGroup "rotate tests" [finite_tests]

main :: IO()
main = defaultMain rotateTestts