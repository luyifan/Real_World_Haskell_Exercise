-- file: mySecond.hs
mySecond :: [a] -> a
mySecond xlist = if null( tail xlist )
		then error "the list is to short"
		else head ( tail xlist )

safeSecond :: [a] -> Maybe a 
safeSecond xlist = if null ( tail xlist )
			then  Nothing
			else Just (head ( tail xlist ))
tidySecond :: [a] -> Maybe a 
tidySecond (_:x:_) = Just x
tidySecond _ = Nothing

