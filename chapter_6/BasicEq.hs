import Data.Char
data Color = Red
	   | Green
	   | Blue
class BasicEq a where
	isEqual :: a -> a -> Bool
	isEqual x y = not (isNotEqual x y )
	isNotEqual :: a -> a -> Bool 
	isNotEqual x y = not ( isEqual x y )
instance BasicEq Bool where
	isEqual True True = True
	isEqual False False = True
	isEqual _ _ = False

instance BasicEq Color where 
	isEqual Red Red = True
	isEqual Blue Blue = True
	isEqual Green Green = True
	isEqual _ _ = False

instance Show Color where
	show Red = "Color 1: Red"
	show Blue = "Color 2: Blue"
	show Green = "Color 3: Green"

instance Read Color where 
	readsPrec x (s:xs) 
		| isSpace s = readsPrec x xs
	readsPrec _ value =
		tryParse [("Red",Red),("Green",Green),("Blue",Blue)]
		where tryParse [] = []  
		      tryParse ((attempt,result):xs) = 
		      	if (take (length attempt) value) == attempt
		 	then [(result,drop (length attempt) value )]
			else tryParse xs
--ghci> (read "Red")::Color Red
--ghci> (read "Green")::Color Green
--ghci> (read "Blue")::Color Blue
--ghci> (read "[Red]")::[Color] [Red]
--ghci> (read "[Red,Red,Blue]")::[Color] [Red,Red,Blue]
instance Eq Color where 
        Red == Red = True
        Blue == Blue = True
	Green == Green = True
	_ == _ = False
