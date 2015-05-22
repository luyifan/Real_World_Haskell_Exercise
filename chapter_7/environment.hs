-- file: environment 
import System.Environment
main = do 
     environment <- getEnvironment  -- IO [(String,String)]
     putStrLn $ show $ lookup "HOME" environment   -- Maybe String 
     environment_exception <- getEnv "Home"
     putStrLn $ show environment_exception 
