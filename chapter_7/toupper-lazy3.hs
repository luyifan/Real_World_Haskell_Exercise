-- file: toupper-lazy3.hs

import Data.Char ( toUpper )

main :: IO()
main = do
	content <- readFile "input.txt" 
	writeFile "output.txt" (map toUpper content)
