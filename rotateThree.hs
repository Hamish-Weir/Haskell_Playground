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
