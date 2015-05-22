-- file: toupper-lazy5.hs
import Data.Char (toUpper)

main :: IO()
main = interact (map toUpper . (++) "Your data, in uppercase, is:\n\n" )
