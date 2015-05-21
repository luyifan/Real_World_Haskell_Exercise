-- file: callingpure.hs

name2reply :: String -> String
name2reply name = 
	"Pleased to meet you, " ++ name ++ ".\n" ++ 
	"You  name contains" ++ charcount ++ " characters"
	where charcount = show( length name )


main :: IO()
main = do 
	putStrLn "Greeting once again, What is you name?"
	input <- getLine
	let outStr = name2reply input 
     	putStrLn outStr 
