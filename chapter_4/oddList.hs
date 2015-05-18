-- file: oddList.hs
oddList :: [ Int ] -> [ Int ]

oddList [] = []
oddList (x:xlist)
	| odd x = x : oddList xlist
	| otherwise = oddList xlist


oddList2 xlist = filter odd xlist

