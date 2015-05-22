-- file: action.hs

string2action :: String -> IO()
string2action input = putStrLn( "Data " ++ input )

list2actions :: [String] -> [IO()]
list2actions = map string2action 

number :: [Int]
number = [1..10]

strings :: [String]
strings = map show number 

actions :: [IO()]
actions = list2actions strings 

printitall :: IO()
printitall = runall actions

runall :: [IO()] -> IO()
runall [] = return ()
runall (firstelem:remainingelem) = do firstelem 
				      runall remainingelem 

main = do string2action "Start of the program" 
	  printitall 
	  string2action "Done!"

