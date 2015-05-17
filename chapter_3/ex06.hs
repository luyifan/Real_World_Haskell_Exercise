-- file: ex06.hs
--Create a function that sorts a list of lists based on the length of each
--sublist. (You may want to look at the sortBy function from the Data.List
--module.)

import Data.List 
sortByLength :: [[a]] -> [[a]]
sortByLength xlist = sortBy compareLength xlist
	where compareLength alist blist 
       		| length alist < length blist = LT
		| length alist == length blist = EQ
		| otherwise = GT

