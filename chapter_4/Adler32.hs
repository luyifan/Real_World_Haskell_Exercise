-- file: Adler.hs
import Data.Bits (shiftL,(.&.),(.|.))
import Data.Char (ord)
base = 65521

adler32 xlist = compute 1 0 xlist
compute a b xlist
	| null xlist = ( shiftL b 16 ) .|.  a
	| otherwise = compute ( ( a + ( ord ( head xlist ) .&. 0xff ) ) `mod` base ) ( ( a + b ) `mod` base ) (tail xlist)
