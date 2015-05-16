-- file: BogusPattern.hs

data Fruit = Apple | Orange | Nil
	   deriving(Show)
apple = "apple"
orange = "orange"

whichFruit :: String -> Fruit

whichFruit f = case f of 
			 "apple" -> Apple
			 "orange" -> Orange
			 _ -> Nil

equational apple = Apple 
equational orange = Orange
