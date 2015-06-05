-- file: Barcode.hs
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
	where products = mapEveryOther (*3) (reverse ds)

-- zipWith :: ( a -> b -> c ) -> [a] -> [b] -> [c]
-- ($) :: ( a -> b ) -> a -> b 
-- then same as follow ($) :: (x -> y) -> x -> y 
-- so (x->y) is a , x is b , y is c 
-- so zipWith ($) :: [x -> y ] -> [x] -> [y]
-- the same as follow [ a -> b ] -> [a] -> [b]
-- cycle :: [a] -> [a] 
-- cycle [ a -> b ] 
-- 接受有限的函数串，创造无限循环的函数串
-- 和zipWith结合使用，可以使得长度为后面接的串的长度
mapEveryOther :: ( a -> a ) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle[f,id])

