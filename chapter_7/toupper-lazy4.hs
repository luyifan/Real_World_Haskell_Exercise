-- file: toupper-lazy4.hs

import Data.Char (toUpper)

main ::IO()
main = interact (map toUpper)
