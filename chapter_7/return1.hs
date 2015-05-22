-- file: return1.hs

import Data.Char(toUpper)

isGreen :: IO Bool
isGreen = do
	putStrLn "Is green your favorite color?" 
	input <- getLine 
	return (  ( toUpper $ head input) == 'Y' ) 

isYes :: String -> Bool 
isYes input = ( toUpper . head $ input ) == 'Y'

isGreen2 :: IO Bool
isGreen2 = do 
	putStrLn "Is green your favorite color?"
	input <- getLine
	return $ isYes input 


returnTest :: IO()
returnTest = do 
	one <- return 1
	-- return 1 
	-- IO Int 
	-- Int <- IO Int
	-- <- 把东西从Monad里面拿出来，实际上就是 return 的反作用
	let two = 2
        putStrLn $ show (one + two)
