-- file: basicio-nodo.hs
main = 
	putStrLn "Greetings!  What is your name?" >>
	getLine >>=
	(\input -> putStrLn $ "Welcome to Haskell, " ++ input ++ "!")
	--ghci> :type (>>)
	--(>>) :: (Monad m) => m a -> m b -> m b
	--ghci> :type (>>=)
	--(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
	--IO() >> 
	--IO String >>=
	--(String -> IO() )  
