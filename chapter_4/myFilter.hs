-- file: myFilter.hs
myFilter p xs =foldr step [] xs
	where step x ys 
       		| p x = x : ys 
		| otherwise = ys
