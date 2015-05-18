-- file: suffixes.hs
import Data.List
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes [] = []

suffixe2 xs= init (tails xs ) 

