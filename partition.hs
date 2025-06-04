partition :: (a -> Bool) -> [a] -> ([a],[a])

partition _ [] = ([],[])
partition pred (x:xs) =
  let (yes, no) = partition pred xs
  in if pred x
     then (x : yes, no)
     else (yes, x : no)


main = do
  print $ partition odd [] == ([],[])
  print $ partition odd [1..10] == ([1,3,5,7,9],[2,4,6,8,10])
  print $ partition even [1..10] == ([2,4,6,8,10],[1,3,5,7,9])
  print $ partition (== 3) [1..5] == ([3],[1,2,4,5])
  print $ partition (== 3) [5,7,2] == ([],[5,7,2])
  print $ partition (== 3) [3,3,3] == ([3,3,3],[])
  print $ partition (== 3) [3,3,4,3] == ([3,3,3],[4])

  print $ partition (\x -> (x^2 `mod` 3)  == 0) [1..5] == ([3],[1,2,4,5])