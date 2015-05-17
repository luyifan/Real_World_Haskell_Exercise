-- file: ex07.hs
-- Define a function that joins a list of lists together using a separator
-- value
intersperse_1 :: a -> [[a]] -> [a]
intersperse_1 separator xlist 
	| null xlist = []
	| length(xlist) == 1 = head xlist
	| otherwise = head xlist ++ [separator] ++ (intersperse_1 separator (tail xlist))
