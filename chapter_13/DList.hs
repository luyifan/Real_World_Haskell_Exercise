-- file: DList.hs
module DList (
       DList ,
       fromList ,
       toList ,
       empty , 
       append , 
       cons , 
       dfoldr ,
       safeHead 
) where 
--把函数当成数据来用
newtype DList a = DL {
	unDL :: [a] -> [a]		  
}

append :: DList a -> DList a -> DList a 
append xs ys = DL ( unDL xs . unDL ys )

fromList :: [a] -> DList a 
fromList xs = DL (xs ++ )

toList :: DList a -> [a]
toList (DL xs) = xs []

empty :: DList a 
empty = DL id 

cons :: a -> DList a -> DList a 
cons x (DL xs) = DL ((x:) . xs)
infixr `cons` 

dfoldr :: ( a -> b -> b ) -> b -> DList a -> b
dfoldr f z xs = foldr f z (toList xs)

-- 尽管 DList 使得拼接很廉价，（由于haskell的惰性决定）但并不是所有的列表操作都容易实现。 列表的
-- head 函数具有常数开销，而对应的 DList 实现却需要将整个 DList
-- 转为普通列表，因此它比普通列表的实现昂贵得多： 它的开销正比于构造 DList
-- 所需的拼接次数。
safeHead :: DList a -> Maybe a 
safeHead xs = case toList xs of 
		(y:_) -> Just y
		_ -> Nothing

dmap :: (a -> b) -> DList a -> DList b 
dmap f = dfoldr go empty 
    where go x = cons (f x)  

instance Functor DList where 
	fmap = dmap 

