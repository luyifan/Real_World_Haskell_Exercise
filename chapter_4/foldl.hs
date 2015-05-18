-- file: foldl.hs
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl step zero (x:xlist) = foldl step ( step zero x ) xlist
foldl _ zero [] = zero

