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