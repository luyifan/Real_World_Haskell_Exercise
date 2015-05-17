-- file: ex04.hs
-- Turn a list into a palindrome; i.e., it should read the same both
-- backward and forward. For example, given the list [1,2,3], your function
-- should return [1,2,3,3,2,1].

palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xlist) = x: palindrome ( xlist ) ++ [x]

-- Not repeating the middle entry such that [1,2,3] becomes [1,2,3,2,1]
palindrome_odd :: [a] -> [a]
palindrome_odd [] = []
palindrome_odd xlist = xlist ++ tail(reverse xlist)
