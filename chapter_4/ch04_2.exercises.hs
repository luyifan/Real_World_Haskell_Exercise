-- file: ch04_2.exercises
-- Use a fold (choosing the appropriate fold will make your code much
-- simpler) to rewrite and improve upon the asInt function from the earlier
-- section“Explicit Re- cursion” on page 85.
-- ￼￼￼How to Think About Loops | 97
-- -- file: ch04/ch04.exercises.hs asInt_fold :: String -> Int
import Data.Char 
asInt_fold :: String -> Int

asInt_fold [] = 0
asInt_fold (x:xlist) 
	| x == '-' = (-1) * asInt_fold xlist
	| x == '+' = asInt_fold xlist
	| otherwise = foldl step 0 (x:xlist)
      where step x y = x*10 + digitToInt y

-- Extend your function to handle the following kinds of exceptional conditions by calling error:

asInt_fold_error :: String -> Int

asInt_fold_error [] = error "The String is null"
asInt_fold_error ('-':xlist) = - (asInt_fold_error xlist)
asInt_fold_error ('+':xlist) = asInt_fold_error xlist
asInt_fold_error xlist = foldl step 0 xlist
	where step now x 
       		| isDigit x = case lessThanZero of  
				 True -> error  "Int overflow"
				 False -> newNow
		| otherwise = error ("Not a digit char '" ++ [x] ++ "'" )
			where newNow = now * 10 + digitToInt x 
	 		      lessThanZero = newNow < 0 

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Left "null list of digits"
asInt_either ('-':xlist) = neg ( asInt_either xlist )
	where neg (Left acc) = Left acc
       	      neg (Right acc) = Right (- acc)
asInt_either xlist = foldl step (Right 0) xlist
	where 
       	     step (Left now) _ = Left now    
	     step (Right now) x 
       		| isDigit x = case lessThanZero of
			       True -> Left "Int overflow"
			       False -> Right newNow
		| otherwise = Left ("non-digit '" ++ [x] ++ "'")
			where newNow = now * 10 + digitToInt x
	 		      lessThanZero = newNow < 0


--  The Prelude function concat concatenates a list of lists into a single
--  list and has the following type:
--  -- file: ch04/ch04.exercises.hs concat :: [[a]] -> [a]
--  Write your own definition of concat using foldr.

concat_fold :: [[a]] -> [a]
concat_fold xlist = foldr step [] xlist
	where step x y = x ++ y

-- Write your own definition of the standard takeWhile function, first
-- using explicit
-- recursion, and then foldr.
takeWhile_explicit :: (a -> Bool) -> [a] -> [a]
takeWhile_explicit func [] = []
takeWhile_explicit func (x:xlist) 
	| func x = x : takeWhile_explicit func xlist
	| otherwise  = takeWhile_explicit func xlist 

takeWhile_fold func xlist = foldr step [] xlist
	where step x y 
       		| func x = x:y
		| otherwise = y

--The Data.List module defines a function, groupBy, which has the following
--type:
---- file: ch04/ch04.exercises.hs
--groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
--Use ghci to load the Data.List module and figure out what groupBy does,
--then write your own implementation using a fold.

groupBy_fold func xlist = foldr step [] xlist
	where step x [] = [[x]]
       	      step x (y:ylist) 
	      	| func x (head y) = (x:y):ylist
		| otherwise = [x]:y:ylist
--
-- How many of the following Prelude functions can you rewrite using list
-- folds? 
-- • any
-- • cycle 
-- • words
-- • unlines
-- For those functions where you can use either foldl' or foldr, which is
-- more ap- propriate in each case?

any_fold func xlist = foldr step False xlist
	where step x y = (func x) || y

cycle_foldr xlist = foldr (:) (cycle_foldr xlist) xlist

words_fold xlist = first : second 
	where (first , second ) = foldr step ("",[]) xlist
       		where step c (lastWord,rest)
	       		| isSpace c = if null lastWord then ("",rest) else ("",lastWord:rest)
			| otherwise = (c:lastWord,rest)

words_fold2 [] = []
words_fold2 (x:xlist)
	| isSpace x = words_fold2 xlist
words_fold2 xlist = foldr step [] xlist
	where step x [] = [[x]]
	      step x ([]:ylist)
		| isSpace x = []:ylist
		| otherwise = [x]:ylist
	      step x (y:ylist)
	       	| isSpace x = []:y:ylist
		| otherwise = (x:y):ylist


unlines_fold xlist = foldr step [] xlist
	where step x y = x ++ "\n" ++ y
