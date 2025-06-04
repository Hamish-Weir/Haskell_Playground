
data WindowLayout = Window {wname :: String, width :: Int, height :: Int}
 | Horizontal [WindowLayout]
 | Vertical [WindowLayout]
    deriving (Eq, Show)

iconify :: WindowLayout -> WindowLayout
iconify (Window n _ _) = Window n 1 1
iconify (Horizontal ws) = Horizontal (map iconify ws)
iconify (Vertical ws) = Vertical (map iconify ws)


main = do
  print $ iconify (Window "castle" 1280 740) 
                == Window "castle" 1 1

  print $ iconify (Horizontal [Window "castle" 1280 740, Window "bball" 900 900]) 
                == Horizontal [Window "castle" 1 1, Window "bball" 1 1]

  print $ iconify (Vertical []) 
                == Vertical []

  print $ iconify (Horizontal []) 
                == Horizontal []

  print $ iconify (Vertical [Horizontal [Window "castle" 1280 740, Window "bball" 900 900], Vertical [Window "csi" 1000 500]])
                == Vertical [Horizontal [Window "castle" 1 1, Window "bball" 1 1], Vertical [Window "csi" 1 1]]

  print $ iconify (Horizontal [Vertical [Window "csi" 1280 740, Window "daily" 900 900], Horizontal [Window "news" 1000 500, Horizontal [Window "pbs" 800 400]]])
                == Horizontal [Vertical [Window "csi" 1 1, Window "daily" 1 1], Horizontal [Window "news" 1 1, Horizontal [Window "pbs" 1 1]]]