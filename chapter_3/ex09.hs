-- file: ex08.hs 
-- Using the binary tree type that we defined earlier in this chapter,
-- write a function that will determine the height of the tree. The height
-- is the largest number of hops from the root to an Empty. For example,
-- the tree Empty has height zero; Node "x" Empty Empty has height one;
-- Node "x" Empty (Node "y" Empty Empty) has height two; and so on.

data Tree a= Node a (Tree a)(Tree a)
	   | Empty
	   deriving(Show)

heightTree :: Tree a-> Int

heightTree Empty = 0
--heightTree (Node _ Empty Empty) = 1
--heightTree (Node _ leftChild Empty) = 1 + (heightTree leftChild) 
--heightTree (Node _ Empty rightChild) = 1 + (heightTree rightChild)
heightTree (Node _ leftChild rightChild ) = 1 + maxHeight 
					where maxHeight = max ( heightTree leftChild ) ( heightTree rightChild )
