--  file: ValidFunctor.hs
{-# LANGUAGE DatatypeContexts #-}

data Foo a = Foo a 

instance Functor Foo where 
 	fmap f (Foo a) = Foo( f a )

--When we define a new type, we can add a type constraint just after the data keyword
-- Wrong
-- data Eq a => Bar a = Bar a 
-- instance Functor Bar where 
-- 	fmap f (Bar a) = Bar( f a ) 
