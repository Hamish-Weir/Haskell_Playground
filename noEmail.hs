-- (10 points) [UseModels] Consider the type of phone book entries below.
-- data Entry = Record {name :: String, phone :: Integer, email :: String}
-- Write, in Haskell, the function
-- noemails :: [Entry] -> [(String,Integer)]
-- that takes a list of records of type Entry, es, and returns a list of pairs of the name and phone number of
-- each entry, in the same order as in es. The following are examples, written using the Testing module
-- from the homework.

-- tests :: [TestCase [(String,Integer)]]
-- tests =
-- [eqTest (noemails []) "==" []
-- ,eqTest (noemails [Record {name = "Eastman", phone = 3214442211,
-- email = "polaroid@p.com"}])
-- "==" [("Eastman",3214442211)]
-- ,eqTest (noemails [Record {name = "M", phone = 44153543221, email = "m@mi6.uk"}
-- ,Record {name = "Bond", phone = 44007007007, email ="jb@mi6.uk"}])
-- "==" [("M",44153543221),("Bond",44007007007)]
-- ,eqTest (noemails sample)
-- "==" [("Adams",4075551212),("Bullfinch",5155551212)
-- ,("Cassidy",8005551122),("Durham",3059123344),("Eastman",3214442211)]
-- ,eqTest (noemails ((Record {name="Durham",phone=3059123344,
-- email="crash@yahoo.com"}):sample))
-- "==" [("Durham",3059123344),("Adams",4075551212),("Bullfinch",5155551212)
-- ,("Cassidy",8005551122),("Durham",3059123344),("Eastman",3214442211)] ]
-- where sample =
-- [Record {name = "Adams", phone = 4075551212, email = "adams@mail.com"}
-- ,Record {name = "Bullfinch", phone = 5155551212, email = "bf@bf.com"}
-- ,Record {name = "Cassidy", phone = 8005551122, email = "cass@mail.com"}
-- ,Record {name = "Durham", phone = 3059123344, email = "bull@dingers.com"}
-- ,Record {name = "Eastman", phone = 3214442211, email = "polaroid@p.com"}]

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