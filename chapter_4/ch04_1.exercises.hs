-- myself is delete ,so this is the answer by bendoerr
-- but make sure they never fail.
--
-- These ended up being just wrappers.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- Write a function splitWith that acts similarly to words but takes a
-- predicate and a list of any type, and then splits its input list on every
-- element for which the predicate returns False
splitWith _ [] = []
splitWith p xs | null front = splitWith p backTail
               | null back  = [front]
               | otherwise  = front : splitWith p backTail
          where (front, back) = span p xs
                backTail      = tail back

-- Using the command framework from the earlier section “A Simple Command-Line
-- Framework” on page 71, write a program that prints the first word of each
-- line of its input.
l = "abc foo\ndef bar\nhij baz\nklm boo"

firstWords s = unlines (map each (lines s))
    where each [] = []
          each s = head (words s)


-- Write a program that transposes the text in a file. For instance, it should
-- convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
-- convert "say\nhello\n to\nworld!\n" to "sh w\naeto\nylor\n l l\n o d\n !\n"

testContent= "say\nhello\n to\nworld!\n"

converter testContent = unlines ( myTranspose ( lines testContent ) ' ' )
myTranspose :: [[a]] -> a -> [[a]]
myTranspose xlist fill 
	| null xlist = []
	| all null xlist = []
	| otherwise = (map myHead xlist) : myTranspose (map myTail xlist) fill
      where myHead onelist 
	    	| null onelist = fill 
		| otherwise = head onelist 
	    myTail onelist
	    	| null onelist = []
		| otherwise = tail onelist 
-- convert "say\nhello\n to\nworld!\n" to "sh w\naeto\nylor\n"
myTranspose2 :: [[a]] -> [[a]]
myTranspose2 xlist 
	| null xlist = []
	| any null xlist = []
	| otherwise = (map head xlist) : myTranspose2 (map tail xlist)

converter2 content = unlines ( myTranspose2 ( lines content ) )
