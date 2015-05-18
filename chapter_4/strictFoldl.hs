-- file: strictFoldl.hs

-- seq :: a -> b -> 
-- 放在seq的a为需要求值的参数，而不能采用惰性块
foldl' _ zero [] = zero
foldl' step zero (x:xlist) = 
		let new = step zero x 
      		in seq new foldl' step new xlist

