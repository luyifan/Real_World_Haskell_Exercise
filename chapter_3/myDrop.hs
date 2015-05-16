-- file: niceDrop
niceDrop n xs | n <= 0 = xs
niceDrop _ [] = []
niceDrop n (x:xs) = niceDrop (n-1) xs
