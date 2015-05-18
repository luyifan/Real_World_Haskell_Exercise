-- file: myFoldrMap.hs
myFoldrMap f xlist = foldr step [] xlist
	where step x xlist = f x : xlist
