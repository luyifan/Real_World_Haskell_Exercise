-- file: EitherInt 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
--Wrong 
--Either Int 那么就表示这个是Either Int b
--表示a是Int类型，所以对应的函数f这只能是Int类型的对应函数，
--增加了约束条件，是不行的，f函数应该只对没有约束的b类型变量有用
{-
instance Functor (Either Int) where 
	fmap f (Left n) = Left (f n ) 
	fmap _ (Right r) = Right r
-}
--Right 
instance Functor (Either Int) where 
-- 	n :: a 
-- 	r :: b 
	fmap _ (Left n) = Left  n
	fmap f (Right r) = Right (f r)

--Ourfirstruleisfunctorsmustpreserveidentity.Thatis,applyingfmap idtoavalue
--should give us back an identical value:
--ghci> fmap id (Node (Leaf "a") (Leaf "b")) Node (Leaf "a") (Leaf "b")
--Oursecondruleisfunctorsmustbecomposable.Thatis,composingtwousesof fmap
--should give the same result as one fmap with the same functions composed:
--ghci> (fmap even . fmap length) (Just "twelve") Just True
--ghci> fmap (even . length) (Just "twelve")
--Just True
