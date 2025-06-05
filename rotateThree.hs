-- (5 points) [Concepts] [UseModels] Consider the data type Amount defined below.
-- data Amount = Zero | One | Two
-- In Haskell, write the polymorphic function
-- rotate :: Amount -> (a,a,a) -> (a,a,a)
-- which takes an Amount, amt, and a triple of elements of some type, (x,y,z), and returns a triple that is
-- circularly rotated to the left by the number of steps indicated by the English word that corresponds to
-- amt. That is, when amt is Zero, then (x,y,z) is returned unchanged; when amt is One, then (y,z,x) is
-- returned; finally, when amt is Two, then (z,x,y) is returned. The following are examples, written using
-- the Testing module from the homework.

-- tests :: [TestCase Bool]
-- tests =
-- [assertTrue ((rotate Zero (1,2,3)) == (1,2,3))
-- ,assertTrue ((rotate One (1,2,3)) == (2,3,1))
-- ,assertTrue ((rotate Two (1,2,3)) == (3,1,2))
-- ,assertTrue ((rotate Two ("jan","feb","mar")) == ("mar","jan","feb"))
-- ,assertTrue ((rotate One ("jan","feb","mar")) == ("feb","mar","jan"))
-- ,assertTrue ((rotate Zero (True,False,True)) == (True,False,True)) ]

data Amount = Zero | One | Two

rotate::Amount -> (a,a,a) -> (a,a,a)
rotate Zero (a,b,c) = (a,b,c)
rotate One (a,b,c) = rotate Zero (b,c,a)
rotate Two (a,b,c) = rotate One (b,c,a)

main = do
    print $ rotate Zero (1,2,3) == (1,2,3)
    print $ rotate One (1,2,3) == (2,3,1)
    print $ rotate Two (1,2,3) == (3,1,2)

    print $ rotate Zero ("jan","feb","mar") == ("jan","feb","mar")
    print $ rotate One ("jan","feb","mar") == ("feb","mar","jan")
    print $ rotate Two ("jan","feb","mar") == ("mar","jan","feb")

    print $ rotate Zero (True,False,True) == (True,False,True)

    print $ rotate Zero ([],[1],[2]) == ([],[1],[2])
    print $ rotate One ([],[1],[2]) == ([1],[2],[])
    print $ rotate Two ([],[1],[2]) == ([2],[],[1])
