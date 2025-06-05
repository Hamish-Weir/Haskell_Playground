-- (25 points) [UseModels] Consider the data type of quantified Boolean expressions defined as follows.
-- data QBExp = Varref String | QBExp `And` QBExp | Forall String QBExp
-- Your task is to write a function
-- freeQBExp :: QBExp -> [String]
-- that takes a QBExp, qbe, and returns a list containing just the strings that occur as a free variable
-- reference in qbe. The following defines what “occurs as a free variable reference” means. A string s
-- occurs as a variable reference in a QBExp if s appears in a subexpression of the form (Varref s). Such
-- a string occurs as a free variable reference if it occurs as a variable reference in a subexpression that is
-- outside of any expression of the form (Forall s e), which declares s. The following are examples that
-- use the course’s Testing module. Note that the lists returned by freeQBExp should have no duplicates.
-- In the tests, the setEq function constructs a test case that considers lists of strings to be equal if they
-- have the same elements (so that the order is not important).

-- tests :: [TestCase [String]]
-- tests = [setEq (freeQBExp (Varref "x")) "==" ["x"]
-- ,setEq (freeQBExp ((Varref "x") `And` (Varref "y"))) "==" ["x","y"]
-- ,setEq (freeQBExp ((Varref "y") `And` (Varref "x"))) "==" ["y","x"]
-- ,setEq (freeQBExp (((Varref "y") `And` (Varref "x"))
-- `And` ((Varref "x") `And` (Varref "y"))))
-- "==" ["y","x"]
-- ,setEq (freeQBExp (Forall "y" (Varref "y"))) "==" []
-- ,setEq (freeQBExp (Forall "y" ((Varref "y") `And` (Varref "z"))))
-- "==" ["z"]
-- ,setEq (freeQBExp (Forall "z" (Forall "y" ((Varref "y") `And` (Varref "z")))))
-- "==" []
-- ,setEq (freeQBExp ((Varref "z")
-- `And` (Forall "z" (Forall "y" ((Varref "y") `And` (Varref "z"))))))
-- "==" ["z"]
-- ,setEq (freeQBExp (((Varref "z") `And` (Varref "q"))
-- `And` (Forall "z" (Forall "y" ((Varref "y") `And` (Varref "z"))))))
-- "==" ["z","q"] ]
-- where setEq = gTest setEqual
-- setEqual los1 los2 = (length los1) == (length los2)
-- && subseteq los1 los2
-- subseteq los1 los2 = all (\e -> e `elem` los2) los1

import Data.List (union)

data QBExp = Varref String | QBExp `And` QBExp | Forall String QBExp

freeQBExp :: QBExp -> [String]
freeQBExp (Forall x y) = filter (/= x) (freeQBExp y)
freeQBExp (Varref a) = [a]
freeQBExp (a `And` b) = (union (freeQBExp a) (freeQBExp b))

main = do
  print $ setEqual (freeQBExp (Varref "x")) ["x"]
  print $ setEqual (freeQBExp ((Varref "x") `And` (Varref "y"))) ["x", "y"]
  print $ setEqual (freeQBExp ((Varref "y") `And` (Varref "x"))) ["y", "x"]
  print $ setEqual (freeQBExp (((Varref "y") `And` (Varref "x")) `And` ((Varref "x") `And` (Varref "y")))) ["y", "x"]
  print $ setEqual (freeQBExp (Forall "y" (Varref "y"))) []
  print $ setEqual (freeQBExp (Forall "y" ((Varref "y") `And` (Varref "z")))) ["z"]
  print $ setEqual (freeQBExp (Forall "z" (Forall "y" ((Varref "y") `And` (Varref "z"))))) []
  print $ setEqual (freeQBExp ((Varref "z") `And` (Forall "z" (Forall "y" ((Varref "y") `And` (Varref "z")))))) ["z"]
  print $ setEqual (freeQBExp (((Varref "z") `And` (Varref "q")) `And` (Forall "z" (Forall "y" ((Varref "y") `And` (Varref "z")))))) ["z", "q"]

setEqual :: Eq a => [a] -> [a] -> Bool
setEqual xs ys = length xs == length ys && all (`elem` ys) xs