-- return the element before the last
lastButOne :: [a] -> Maybe a
lastButOne [] = Nothing
lastButOne (x:[]) = Nothing
lastButOne (x:y:[]) = Just x
lastButOne (x:xlist) = lastButOne xlist
