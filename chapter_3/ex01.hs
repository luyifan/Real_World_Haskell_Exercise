-- file: ex01.hs
-- get the length of list
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xlist) = 1 + listLength xlist 
