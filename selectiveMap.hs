-- (15 points) [Concepts] [UseModels] Without using any functions from the Haskell prelude, write the
-- polymorphic function
-- selectiveMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
-- which takes a predicate, p, a function f, and a list lst, and returns a list that is just like lst, but in
-- which every element x that satisfies p is replaced by f applied to x. (An element x satisfies p if
-- (p x) == True.) The following are examples, written using the Testing module from the homework.

-- tests :: [TestCase Bool]
-- tests =
-- [assertTrue ((selectiveMap odd (\x -> x*x) []) == [])
-- ,assertTrue ((selectiveMap odd (\x -> 6000) [2,4,6,8,10]) == [2,4,6,8,10])
-- ,assertTrue ((selectiveMap odd (\x -> 6000) [3,1,-5]) == [6000,6000,6000])
-- ,assertTrue ((selectiveMap even (\x -> x*x) [1,2,3,4,5]) == [1,4,3,16,5])
-- ,assertTrue ((selectiveMap (\c -> c == 'q') (\x -> 'u') "quip") == "uuip")
-- ,assertTrue ((selectiveMap (== True) not [True,False,True]) == [False,False,False]) ]

selectiveMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
selectiveMap pred fun = map (\n -> if pred n then fun n else n)

main = do
  print $ selectiveMap odd (\x -> x*x) [] == []
  print $ selectiveMap odd (\x -> 6000) [2,4,6,8,10] == [2,4,6,8,10]
  print $ selectiveMap odd (\x -> 6000) [3,1,-5] == [6000,6000,6000]
  print $ selectiveMap even (\x -> x*x) [1,2,3,4,5] == [1,4,3,16,5]
  print $ selectiveMap (== 'q') (const 'u') "quip" == "uuip"
  print $ selectiveMap (== True) not [True,False,True] == [False,False,False]