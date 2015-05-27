-- file: TyeConstraint.hs
-- Adding a constraint to a type definition is essentially never a good
-- idea. It has the effect of forcing you to add type constraints to every
-- function that will operate on values of that type. 
{-# LANGUAGE DatatypeContexts #-}
data (Ord a) => OrdStack a = Bottom 
	     		   | Item a ( OrdStack a )
			   deriving (Show)
--If we want to write a function that checks the stack to see whether it is
--increasing (i.e., every element is bigger than the element below it),
--we’ll obviously need an Ord constraint to perform the pairwise comparisons
isIncreasing :: (Ord a) => OrdStack a -> Bool 
isIncreasing (Item a rest@(Item b _))
	| a < b = isIncreasing rest 
	| otherwise = False 
isIncreasing _ = True 

push :: (Ord a) => a -> OrdStack a -> OrdStack a 
push a s = Item a s

