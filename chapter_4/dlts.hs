-- file: dlts.hs
import Data.List (isPrefixOf)

dlts:: String -> [String]

dlts = foldr step [] . lines 
	where step x y 
       		| "#define DLT_" `isPrefixOf` x = secondWord x : y
		| otherwise = y
	      secondWord = head . tail . words

dlts2 = map ( head . tail .  words ) . filter ("#define DLT_" `isPrefixOf` ) . lines
