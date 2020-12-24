module Test2 (tests2) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.Hedgehog as HH
import Test.Tasty.HUnit

import Part2.Types
import Part2

tests2 :: [TestTree]
tests2 =
  [ test6
  , test7
  , test8
  , test9
  , test10
  , test11
  , test12
  , test13
  , test14
  , test15
  , test16
  , test17
  ]

test6 :: TestTree
test6 = testGroup "P06"
  [ testCase "prob6 RED == 'R'" $ prob6 RED @?= 'R' ]

test7 :: TestTree
test7 = testGroup "P07"
  [ testCase "prob7 (Red 300) == F" $ prob7 (Red 300) @?= False
  , testCase "prob7 (Red 10)  == T" $ prob7 (Red 10)  @?= True
  ]

test8 :: TestTree
test8 = testGroup "P08"
  [ testCase "prob8 (100,100,100) (Red 10) == (110,100,100)" $
    prob8 (Color 100 100 100) (Red 10) @?= Color 110 100 100
  , testCase "prob8 (200,150,100) (Green -50) == (200,100,100)" $
    prob8 (Color 200 150 100) (Green (-50)) @?= Color 200 100 100
  ]

test9 :: TestTree
test9 = testGroup "P09"
  [ testCase "prob9 (Red 100) == 100" $ prob9 (Red 100) @?= 100
  , testCase "prob9 (Blue 50) == 50"  $ prob9 (Blue 50) @?= 50
  ]

test10 :: TestTree
test10 = testGroup "P10"
  [ testCase "prob10 (0,0,50) == Just (B 50)" $
    prob10 (Color 0 0 50) @?= Just (Blue 50)
  , testCase "prob10 (0,50,0) == Just (G 50)" $
    prob10 (Color 0 50 0) @?= Just (Green 50)
  , testCase "prob10 (0,50,50) == Nothing" $
    prob10 (Color 0 50 50) @?= Nothing
  , testCase "prob10 (0,50,100) == Just (B 100)" $
    prob10 (Color 0 50 100) @?= Just (Blue 100)
  , testCase "prob10 (0,100,50) == Just (G 100)" $
    prob10 (Color 0 100 50) @?= Just (Green 100)
  , testCase "prob10 (50,0,0) == Just (R 50)" $
    prob10 (Color 50 0 0) @?= Just (Red 50)
  , testCase "prob10 (50,0,100) == Just (B 100)" $
    prob10 (Color 50 0 100) @?= Just (Blue 100)
  , testCase "prob10 (100,50,0) == Just (R 100)" $
    prob10 (Color 100 50 0) @?= Just (Red 100)
  , testCase "prob10 (100,50,100) == Nothing" $
    prob10 (Color 100 50 100) @?= Nothing
  , testCase "prob10 (100,100,100) == Nothing" $
    prob10 (Color 100 100 100) @?= Nothing
  ]

branch :: Maybe (Tree a) -> a -> Maybe (Tree a) -> Maybe (Tree a)
branch left root right = Just $ Tree left root right

leaf :: a -> Maybe (Tree a)
leaf root = branch Nothing root Nothing

-- (1 2 (3 4 (5 6 nil)))
--   2
--  / \
-- 1   4
--    / \
--   3   6
--      /
--     5
tree1 :: Tree Int
tree1 = Tree (Just $ Tree Nothing 1 Nothing)
             2
             (Just $ Tree (Just $ Tree Nothing 3 Nothing)
                          4
                          (Just $ Tree (Just $ Tree Nothing 5 Nothing)
                                  6
                                  Nothing))

-- ((1 2 3) 4 (5 6 nil))
--      4
--     / \
--    /   \
--   2     6
--  / \   /
-- 1   3 5
tree2 :: Tree Int
tree2 = Tree (Just $ Tree (Just $ Tree Nothing 1 Nothing)
                          2
                          (Just $ Tree Nothing 3 Nothing))
             4
             (Just $ Tree (Just $ Tree Nothing 5 Nothing)
                          6
                          Nothing)

-- (2 3 1)
--   3
--  / \
-- 2   1
tree3 :: Tree Int
tree3 = Tree (Just $ Tree Nothing 2 Nothing) 3 (Just $ Tree Nothing 1 Nothing)

tree4 :: Tree ()
tree4 = Tree (Just $ Tree Nothing () Nothing)
             ()
             (Just $ Tree Nothing () Nothing)

-- ((x x nil) x nil)
--     X
--    /
--   X
--  /
-- X
tree5 :: Tree ()
tree5 = Tree (Just $ Tree (Just $ Tree Nothing () Nothing)
                          ()
                          Nothing)
             ()
             Nothing

tree6Str :: String
tree6Str = "((nil 1 2) 2 3)"
--    2
--   / \
--  /   \
-- 1     3
--  \
--   2
tree6 :: Tree Int
tree6 = Tree
	(branch Nothing
	        1
	        (leaf 2))
	2
	(leaf 3)

tree7Str :: String
tree7Str = "(1 2 (2 3 nil))"
--    2
--   / \
--  /   \
-- 1     3
--      /
--     2
tree7 :: Tree Int
tree7 = Tree
	(leaf 1)
	2
	(branch (leaf 2)
	        3
	        Nothing)

tree8Str :: String
tree8Str = "(1 2 (1 3 nil))"
--    2
--   / \
--  /   \
-- 1     3
--      /
--     1
tree8 :: Tree Int
tree8 = Tree
	(leaf 1)
	2
	(branch (leaf 1)
	        3
	        Nothing)

tree9Str :: String
tree9Str = "(nil 4 (nil 6 8))"
-- 4
--  \
--   6
--    \
--     8
tree9 :: Tree Int
tree9 = Tree
	Nothing
	4
	(branch Nothing
	        6
	        (leaf 8))

tree10Str :: String
tree10Str = "(4 6 8)"
--   6
--  / \
-- 4   8
tree10 :: Tree Int
tree10 = Tree
	(leaf 4)
	6
	(leaf 8)

tree11Str :: String
tree11Str = "(nil 5 nil)"
-- 5
tree11 :: Tree Int
tree11 = Tree Nothing 5 Nothing

test11 :: TestTree
test11 = testGroup "P11"
  [ testCase "prob11 (1 2 (3 4 (5 6 nil))) == 21" $ prob11 tree1 @?= 21
  , testCase "prob11 ((1 2 3) 4 (5 6 nil)) == 21" $ prob11 tree2 @?= 21
  , testCase "prob11 (3 1 2) == 6" $ prob11 tree3 @?= 6
  ]

test12 :: TestTree
test12 = testGroup "P12"
  [ testCase "prob12 (1 2 (3 4 (5 6 nil))) == T" $ prob12 tree1 @?= True
  , testCase "prob12 ((1 2 3) 4 (5 6 nil)) == T" $ prob12 tree2 @?= True
  , testCase "prob12 (3 1 2) == F" $ prob12 tree3 @?= False
  , testCase ("prob12 " ++ tree6Str ++ " == F") $ prob12 tree6 @?= False
  , testCase ("prob12 " ++ tree7Str ++ " == T") $ prob12 tree7 @?= True
  , testCase ("prob12 " ++ tree8Str ++ " == F") $ prob12 tree8 @?= False
  ]

test13 :: TestTree
test13 = testGroup "P13"
  [ testCase "prob13 6 (1 2 (3 4 (5 6 nil))) == Just (5 6 nil)" $
    prob13 6 tree1 @?= Just (Tree (Just $ Tree Nothing 5 Nothing) 6 Nothing)
  , testCase "prob13 7 ((1 2 3) 4 (5 6 nil)) == Nothing" $
    prob13 7 tree2 @?= Nothing
  ]

test14 :: TestTree
test14 = testGroup "P14"
  [ testCase "prob14 (x x x) == (2 3 1)" $
    prob14 tree4 @?= tree3
  , testCase "prob14 (nil x (x x x)) == (nil 4 (2 3 1))" $
    prob14 (Tree Nothing () (Just tree4)) @?= (Tree Nothing 4 (Just tree3))
  , testCase "prob14 ((x x x) x (x x nil)) == ((4 5 3) 6 (1 2 nil))" $
    prob14
        (Tree (branch (leaf ())
                      ()
                      (leaf ()))
              ()
              (branch (leaf ())
                      ()
                      Nothing))
        @?=
        Tree (branch (leaf 4)
                     5
                     (leaf 3))
             6
             (branch (leaf 1)
                     2
                     Nothing)
  ]

test15 :: TestTree
test15 = testGroup "P15"
  [ testCase "prob15 (1 2 (3 4 (5 6 nil))) == ((1 2 3) 4 (5 6 nil))" $
    prob15 tree1 @?= tree2
  , testCase "prob15 (x x x) == ((x x nil) x nil)" $
    prob15 tree4 @?= tree5
  , testCase ("prob15 " ++ tree9Str ++ " == " ++ tree10Str) $
    prob15 tree9 @?= tree10
  , testCase "prob15 (nil x x) == (x x nil)" $
    prob15 (Tree Nothing () (leaf ())) @?= (Tree (leaf ()) () Nothing)
  , testCase ("prob15 " ++ tree11Str ++ " == " ++ tree11Str) $
    prob15 tree11 @?= tree11
  ]

test16 :: TestTree
test16 = testGroup "P16"
  [ testCase "prob16 ((1 2 3) 4 (5 6 nil)) == (1 2 (3 4 (5 6 nil)))" $
    prob16 tree2 @?= tree1
  , testCase "prob16 ((x x nil) x nil) == (x x x)" $
    prob16 tree5 @?= tree4
  , testCase ("prob16 " ++ tree10Str ++ " == " ++ tree9Str) $
    prob16 tree10 @?= tree9
  , testCase "prob16 (x x nil) == (nil x x)" $
    prob16 (Tree (leaf ()) () Nothing) @?= (Tree Nothing () (leaf ()))
  , testCase ("prob16 " ++ tree11Str ++ " == " ++ tree11Str) $
    prob16 tree11 @?= tree11
  ]

test17 = testGroup "P17"
  [ testCase "prob17 (1 2 (3 4 (5 6 nil))) == ((1 2 3) 4 (5 6 nil))" $
    prob17 tree1 @?= tree2
  , testCase "prob17 ((x x nil) x nil) == (x x x)" $
    prob17 tree5 @?= tree4
  ]
