-- file: TreeMap.hs
import Control.Monad ( Functor ) 
data Tree a = Node (Tree a)(Tree a)
	    | Leaf a 
	    deriving (Show)
treeLengths ( Leaf s ) = Leaf (length s)
treeLengths (Node l r ) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b ) -> Tree a -> Tree b 
treeMap f ( Leaf a ) = Leaf (f a )
treeMap f ( Node l r) = Node ( treeMap f l ) ( treeMap f r ) 

-- typeclass Functor
-- class Functor f where
-- 	fmap :: (a -> b) -> f a -> f b

instance Functor Tree where 
	fmap = treeMap 
--  we can only make instances of Functor from types that have exactly one
--  type parameter.
--  We can’t write an fmap implementation for Either a b or (a, b), for
--  example, because these have two type parameters. We also can’t write
--  one for Bool or Int, as they have no type parameters.


--instance Functor Maybe where 
--	fmap _ Nothing = Nothing 
--	fmap f (Just x) = Just (f x)
