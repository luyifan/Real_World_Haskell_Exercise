-- file: ListADT.hs
data List a = Cons a (List a)
	    | Nil
	    deriving (Show)
fromList [] = Nil 
fromList (x:xlist) = Cons x ( fromList xlist)

toList Nil = []
toList (Cons a b) = a: toList b 
