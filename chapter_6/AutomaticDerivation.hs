-- file: AutomaticDerivation.hs
data CannotShow = CannotShow 
		deriving (Show)
data CannotDerivieShow = CannotDerivieShow CannotShow
		       deriving (Show)

data OK = OK

instance Show OK where
	show _ = "OK"

data ThisWorks = ThisWorks OK
	       deriving (Show)
