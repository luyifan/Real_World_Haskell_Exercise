-- file: foldlSum.hs

foldlSum xlist = foldl (+) 0 xlist

foldlSum2 xlist = foldl step 0 xlist
	where step nowans x = nowans + x 
