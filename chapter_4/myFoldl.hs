-- file: myFoldl.hs

myFoldl f zero xs = (foldr step id xs) zero
	where step x g a = g ( f a x )
-- myFoldl (+) 0 [1, 2] =
--
-- (foldr step id [1, 2]) 0
-- step 1 (foldr step id [2]) 0
-- step 1 (step 2 (foldr step id [])) 0
-- step 1 (step 2 id) 0
-- (step 2 id) ((+) 0 1)
-- id ((+) ((+) 0 1) 2)
-- 3

