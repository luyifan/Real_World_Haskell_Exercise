-- realize drop function
myDrop :: Int -> [a] -> [a]
myDrop n xlist = if n <= 0 || null xlist 
			 then xlist
			 else myDrop (n-1) (tail xlist)

