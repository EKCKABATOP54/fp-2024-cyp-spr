
import Btree

import Test.Tasty
import Test.Tasty.HUnit


testBTree :: Tree Char
testBTree = Node (Node (Node Leaf 'A' Leaf) 'B' (Node (Node Leaf 'C' Leaf) 'D' (Node Leaf 'E' Leaf))) 'F' (Node Leaf 'G' (Node (Node Leaf 'H' Leaf) 'I' Leaf))

in_order_test :: TestTree
in_order_test = testGroup "in_order tests"
  [ testCase "Base test" $ assertEqual "Base test" (in_order testBTree) "ABCDEFGHI"
  ]

pre_order_test :: TestTree
pre_order_test = testGroup "pre_order tests"
  [ testCase "Base test" $ assertEqual "Base test" (pre_order testBTree) "FBADCEGIH"
  ]

post_order_test :: TestTree
post_order_test = testGroup "post_order tests"
  [ testCase "Base test" $ assertEqual "Base test" (post_order testBTree) "ACEDBHIGF"
  ]



bTreeTests :: TestTree
bTreeTests = testGroup "BTree tests" [in_order_test, pre_order_test, post_order_test]

main :: IO()
main = defaultMain bTreeTests