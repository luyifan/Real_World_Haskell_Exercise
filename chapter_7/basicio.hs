-- file: basicio.h

writefoo = putStrLn "Greeting! What is your name?"
main = do 
    writefoo 
    inpStr <- getLine
    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
