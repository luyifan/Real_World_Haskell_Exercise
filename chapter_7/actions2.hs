-- file: actions2.hs

str2action :: String -> IO()
str2action input = putStrLn ( "Data " ++ input ) 

number :: [Int]
number = [1..10]

main = do 
	  str2action "Program Start Here"
	  mapM_ (str2action . show) number 
	  str2action "Done!"
