selectiveMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
selectiveMap pred fun = map (\n -> if pred n then fun n else n)

main = do
  print $ selectiveMap odd (\x -> x*x) [] == []
  print $ selectiveMap odd (\x -> 6000) [2,4,6,8,10] == [2,4,6,8,10]
  print $ selectiveMap odd (\x -> 6000) [3,1,-5] == [6000,6000,6000]
  print $ selectiveMap even (\x -> x*x) [1,2,3,4,5] == [1,4,3,16,5]
  print $ selectiveMap (== 'q') (const 'u') "quip" == "uuip"
  print $ selectiveMap (== True) not [True,False,True] == [False,False,False]