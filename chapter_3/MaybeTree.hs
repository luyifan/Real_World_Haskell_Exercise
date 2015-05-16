-- file: MaybeTree.hs

data MaybeTree a= Node (Maybe a) (Maybe (MaybeTree a))(Maybe (MaybeTree a))
		deriving (Show)

empty1 = Node Nothing 
left_node1 = Node (Just "left") Nothing Nothing 
right_node1 = Node (Just "right") Nothing Nothing
parent_node1 = Node (Just "parent") (Just left_node1) (Just right_node1)
data MaybeTree2 a = MaybeTree2 (Maybe (a,MaybeTree2 a,MaybeTree2 a))
		  deriving (Show)
empty = MaybeTree2 Nothing 
left_node = MaybeTree2 (Just ("left",empty,empty))
right_node = MaybeTree2 (Just ("right",empty,empty))
parent_node = MaybeTree2 (Just ("parent",left_node,right_node))



