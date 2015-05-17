-- file: ex05.hs
-- Write a function that determines whether its input list is a palindrome.

isPalindrome :: (Eq a) =>  [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xlist) = (x == last(xlist)) && isPalindrome( take (length(xlist) - 1) xlist )

isPalindrome_2 :: (Eq a) => [a] -> Bool
isPalindrome_2 xlist = xlist == reverse xlist

isPalindrome_3 :: (Eq a) => [a] -> Bool 
isPalindrome_3 [] = True
isPalindrome_3 xlist = firstPart == reverseSecondPart
	where 
       		is_Even = mod ( length xlist ) 2 == 0
		split = splitAt ( floor ( fromIntegral ( length xlist ) / 2 ) ) xlist 
       		firstPart = fst split
		reverseSecondPart = case is_Even of 
				      True ->  reverse (snd split)
				      False -> reverse ( tail ( snd split ) ) 



