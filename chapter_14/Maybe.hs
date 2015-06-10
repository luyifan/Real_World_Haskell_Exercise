-- file: Maybe.hs
-- chain :: m a -> ( a -> m b ) -> m b 
-- inject :: a -> m a
-- define a monad in Haskell. Let's revisit the above list in condensed
-- form.
--
-- A type constructor m.
--
-- A function of type m a -> (a -> m b) -> m b for chaining the output of
-- one function into the input of another.
--
-- A function of type a -> m a for injecting a normal value into the chain,
-- i.e. it wraps a type a with the type constructor m.

class Monad m where 
	--core function 
	--chain
	(>>=) :: m a -> ( a -> m b ) -> m b 
	--inject 
	return :: a -> m a
	-- not core function 
	-- it performs chaining, but it ignores the value on the left.	
	(>>) :: m a -> m b -> m b 
	a >> f = a >>= \_ -> f
	fail :: String -> m a 
	fail = error
--There are a few terms of jargon around monads
--
--“Monadic” simply means “pertaining to monads”. A monadic type is an
--instance of the Monad typeclass; a monadic value has a monadic type.
--
--When we say that a type “is a monad”, this is really a shorthand way of
--saying that it's an instance of the Monad typeclass. Being an instance of
--Monad gives us the necessary monadic triple of type constructor,
--injection function, and chaining function.
--
--In the same way, a reference to “the Foo monad” implies that we're
--talking about the type named Foo, and that it's an instance of Monad.
--
--An “action” is another name for a monadic value. This use of the word
--probably originated with the introduction of monads for I/O, where
--a monadic value like print "foo" can have an observable side effect.
--A function with a monadic return type might also be referred to as an
--action, though this is a little less common.
