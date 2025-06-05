-- (10 points) [UseModels] In Haskell, write the function:
-- squareOdds :: [Integer] -> [Integer]
-- that takes a list of Integers, lst, and returns a list of Integers that is just like lst, except that each odd
-- element of lst is replaced by the square of that element. In your solution, you might find it helpful to
-- use the built-in predicate odd.
-- The following are examples, written using the Testing module from the homework.

-- tests :: [TestCase [Integer]]
-- tests = [eqTest (squareOdds []) "==" []
-- ,eqTest (squareOdds [3]) "==" [9]
-- ,eqTest (squareOdds [4,3]) "==" [4,9]
-- ,eqTest (squareOdds [1,2,3,4,5,6]) "==" [1,2,9,4,25,6]
-- ,eqTest (squareOdds [3,22,3,95,600,0,-1]) "==" [9,22,9,9025,600,0,1]
-- ]

mod2 :: Integer -> Integer
mod2 n = mod n 2

squareOdd :: Integer -> Integer
squareOdd n = if mod2 n == 1 then  n*n
 else n

squareOdds :: [Integer]->[Integer]
squareOdds [] = []
squareOdds (x:xs) = squareOdd x : squareOdds xs

-- squareOdds :: [Integer] -> [Integer]
-- squareOdds = map (\n -> if odd n then n * n else n)

main = do
  print $ squareOdds [] == []
  print $ squareOdds [3] == [9]
  print $ squareOdds [2] == [2]
  print $ squareOdds [-1] == [1]
  print $ squareOdds [-2] == [-2]
  print $ squareOdds [4,3] == [4,9]
  print $ squareOdds [1,2,3,4,5,6] == [1,2,9,4,25,6]
  print $ squareOdds [3,22,3,95,600,0,-1] == [9,22,9,9025,600,0,1]
