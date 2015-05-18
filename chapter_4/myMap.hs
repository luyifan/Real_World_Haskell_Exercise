-- file: myMap.hs
myMap :: (a->b) -> [a] -> [b]
myMap f (x:xlist) = f x : myMap f xlist
myMap _ [] = []
-- myMap _ _ = [] Wrong , the second must be list

