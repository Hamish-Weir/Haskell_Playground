data Entry = Record {name :: String, phone :: Integer, email :: String}

noemails :: [Entry] -> [(String,Integer)]
noemails = map (\r -> (name r, phone r))

sample :: [Entry]
sample =
  [ Record "Adams" 4075551212 "adams@mail.com"
  , Record "Bullfinch" 5155551212 "bf@bf.com"
  , Record "Cassidy" 8005551122 "cass@mail.com"
  , Record "Durham" 3059123344 "bull@dingers.com"
  , Record "Eastman" 3214442211 "polaroid@p.com"
  ]

main = do
  print $ noemails [] == []
  print $ noemails [Record "Eastman" 3214442211 "polaroid@p.com"]
          == [("Eastman",3214442211)]
  print $ noemails [Record "M" 44153543221 "m@mi6.uk", Record "Bond" 44007007007 "jb@mi6.uk"]
          == [("M",44153543221),("Bond",44007007007)]
  print $ noemails sample
          == [("Adams",4075551212),("Bullfinch",5155551212),
               ("Cassidy",8005551122),("Durham",3059123344),("Eastman",3214442211)]
  print $ noemails (Record "Durham" 3059123344 "crash@yahoo.com" : sample)
          == [("Durham",3059123344),("Adams",4075551212),("Bullfinch",5155551212),
               ("Cassidy",8005551122),("Durham",3059123344),("Eastman",3214442211)]