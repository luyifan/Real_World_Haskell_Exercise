-- file: NewType.hs
data DataInt = D Int 
	deriving (Eq,Ord,Show)

newtype NewTypeInt = N Int 
	deriving (Eq,Ord,Show)
newtype UniqueID = UniqueID Int
	deriving (Eq)
