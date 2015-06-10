-- file: Logger.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Logger (
	Logger ,
	Log , 
	runLogger , 
	record , 
	globToRegex 
) where 
newtype Logger a= Logger { execLogger :: (a,Log) }
globToRegex :: String -> Logger String 

type Log = [ String ]

runLogger :: Logger a -> ( a , Log )
runLogger = execLogger  

record :: String -> Logger ()
record s = Logger ((),[s])


globToRegex cs = 
	globToRegex' cs >>= \ds -> 
	return ('^':ds)
globToRegex' :: String -> Logger String 
globToRegex' "" = return "$"
globToRegex' ('?':cs) = 
		record "any" >> 
		globToRegex' cs >>= \ds -> 
		return ('.':ds)
globToRegex' ('*':cs) = do 
	record "kleene star"
	ds <- globToRegex' cs 
	return (".*" ++ ds )

globToRegex' ('[':'!':c:cs) = 
	record "character class , negative" >>
	charClass cs >>= \ds ->
	return ("[^" ++ c:ds)
globToRegex' ('[':c:cs) = 
	record "character clas" >> 
	charClass cs >>= \ds -> 
	return ('[':c:ds)
globToRegex' ('[':_) =
	fail "unterminated character class"

globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)
escape :: Char -> Logger String 
escape c 
	| c `elem` regexChars = record "escape" >> return ['\\',c]
	| otherwise =  return [c]
      where regexChars = "\\+()^$.{}]|"
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c 
liftM2 f m1 m2 = 
	m1 >>= \a ->
	m2 >>= \b ->
	return (f a b)
-- liftM is in Control.Monad 
liftM :: ( Monad m) => ( a -> b ) -> m a -> m b 
liftM f m = m >>= \a -> return (f a)

instance (Monad m) => Functor m where 
	fmap = liftM 
--compare charClass_wordy with charClass
charClass_wordy (']':cs) = 
	globToRegex' cs >>= \ds ->
	return (']':ds)
charClass_wordy (c:cs) = 
	charClass_wordy cs >>= \ds ->
	return (c:ds)
charClass :: String -> Logger String 
charClass (']':cs) = (']':) `liftM` globToRegex' cs 
charClass (c:cs) = (c:) `liftM` globToRegex' cs 

instance Monad Logger where
	--inject 
	return a = Logger (a,[])
	--chain (>>=) :: Logger a -> ( a -> Logger b) -> Logger b 
	m >>= k = let (a,w) = execLogger m 
	       	      n = k a 
		      (b,x) = execLogger n
		  in Logger (b,w ++ x)