-- file: isInAny.hs
import Data.List (isInfixOf)

isInAny needle haystack = any (\s -> needle `isInfixOf` s) haystack
unsafeHead = \(x:_) -> x

isInAny2 needle haystack = any (isInfixOf needle ) haystack

isInAny3 needle = any (needle `isInfixOf`) 


