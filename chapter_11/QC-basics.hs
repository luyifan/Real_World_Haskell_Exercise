-- file: QC-basics.hs
import Test.QuickCheck
import Data.List 
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
	where lhs = filter (<x) xs 
              rhs = filter (>=x) xs

prop_idempotent xs = qsort ( qsort xs ) == qsort xs 
prop_minimum xs = not (null xs) ==> head ( qsort xs ) == minimum xs
prop_ordered xs = ordered ( qsort xs ) 
	where ordered [] = True 
       	      ordered [x] = True 
	      ordered (x:y:xs) = x <= y && prop_ordered (y:xs)
--(\\): 可以得到第二个列表不包含第一个列表得元素的列表
--Main Test.QuickCheck> [1,4] \\ [1,2,3]
--[4]
prop_permutation  xs = permutation xs (qsort xs)
	where permutation xs ys = null ( xs \\ ys ) && null ( ys \\ xs )

prop_maximum xs = not (null xs) ==> last ( qsort xs ) == maximum xs 

prop_append xs ys = 
		not (null xs) ==> not (null ys) ==>
		head ( qsort (xs ++ ys) ) == min ( minimum xs ) ( minimum ys )
prop_append_equal  xs ys = (==>) (null xs ) ( (==>) (null ys) (head ( qsort (xs ++ ys) ) == min ( minimum xs ) ( minimum ys )))
prop_sort_model xs = qsort xs == sort xs 
