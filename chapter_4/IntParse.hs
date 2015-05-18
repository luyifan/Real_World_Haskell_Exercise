-- file: IntParse.hs
import Data.Char (digitToInt)
asInt :: String -> Int
loop :: Int -> String -> Int
asInt xs = loop 0 xs 
loop ans [] = ans 
loop ans (x:xlist) = loop ( ans * 10 + digitToInt x ) xlist 

