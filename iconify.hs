-- (20 points) [UseModels] This problem is about the type WindowLayout, which is defined as follows.
-- data WindowLayout = Window {wname :: String, width :: Int, height :: Int}
-- | Horizontal [WindowLayout]
-- | Vertical [WindowLayout]
-- In Haskell, write a function
-- iconify :: WindowLayout -> WindowLayout
-- that takes a 〈WindowLayout〉, wl, and returns a 〈WindowLayout〉 that is just like wl, except that in each
-- 〈Window〉 record, the value of each width and height field is replaced by 1. The following are
-- examples using the Testing module from the homework.

-- tests :: [TestCase WindowLayout]
-- tests =
-- [eqTest (iconify Window {wname="castle", width=1280, height=740})
-- "==" (Window {wname="castle", width=1, height=1})
-- ,eqTest (iconify (Horizontal [Window {wname="castle", width=1280, height=740},
-- Window {wname="bball", width=900, height=900}]))
-- "==" (Horizontal [Window {wname="castle", width=1, height=1},
-- Window {wname="bball", width=1, height=1}])
-- ,eqTest (iconify (Vertical [])) "==" (Vertical [])
-- ,eqTest (iconify (Horizontal [])) "==" (Horizontal [])
-- ,eqTest (iconify (Vertical [Horizontal [Window {wname="castle", width=1280, height=740},
-- Window {wname="bball", width=900, height=900}],
-- Vertical [Window {wname="csi", width=1000, height=500}]]))
-- "==" (Vertical [Horizontal [Window {wname="castle", width=1, height=1},
-- Window {wname="bball", width=1, height=1}],
-- Vertical [Window {wname="csi", width=1, height=1}]])
-- ,eqTest (iconify (Horizontal [Vertical [Window {wname="csi", width=1280, height=740},
-- Window {wname="daily", width=900, height=900}],
-- Horizontal [Window {wname="news", width=1000, height=500},
-- Horizontal [Window {wname="pbs", width=800,height=400}]]]))
-- "==" (Horizontal [Vertical [Window {wname="csi", width=1, height=1},
-- Window {wname="daily", width=1, height=1}],
-- Horizontal [Window {wname="news", width=1, height=1},
-- Horizontal [Window {wname="pbs", width=1,height=1}]]]) ]
-- Be sure to follow the grammar!

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