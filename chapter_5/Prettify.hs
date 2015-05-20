-- file: Prettify.hs
module Prettify where 
import Numeric (showHex)
import Data.Bits (shiftR,(.&.))
import Data.Char (ord)
data Doc = Empty 
	 | Char Char
	 | Text String
	 | Line 
	 | Concat Doc Doc 
	 | Union Doc Doc 
	deriving (Show)

empty :: Doc 
empty = Empty 

char :: Char -> Doc 
char  = Char  

text :: String -> Doc 
text "" = Empty 
text s = Text s

double :: Double -> Doc
double d = text ( show d ) 

line :: Doc 
line = Line 

string :: String -> Doc
string = enclose '"' '"' .  hcat . map oneChar 

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left  <>  x <> char right 

(<>) :: Doc -> Doc -> Doc
Empty <> y = y 
x <> Empty = x 
x <> y = x `Concat` y



hcat :: [Doc] -> Doc
hcat = foldr (<>) empty  

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of 
		Just r -> text r
		Nothing | mustEscape c -> hexEscape c 
			| otherwise -> char c
	where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char , String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
	where ch a b = (a,['\\',b])

hexEscape :: Char -> Doc 
hexEscape c | d < 0x10000 = smallHex d
	    | otherwise = astral ( d - 0x10000 ) 
	where d = ord c

smallHex :: Int -> Doc
smallHex x = text "\\u" 
	   <> text ( replicate ( 4 - length h ) '0' ) 
	   <> text h
	where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex ( a + 0xd800 ) <> smallHex ( b + 0xdc00 )
	where a = (n `shiftR` 10 ) .&. 0x3ff 
       	      b = n .&. 0x3ff

series :: Char -> Char -> ( a -> Doc ) -> [a] -> Doc
series open close f = enclose open close .  fsep .  punctuate (char ',')  . map f

fsep :: [Doc] -> Doc 
fsep = foldr (</>) empty 

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc 
softline = group line

group :: Doc -> Doc 
group x = flattern x `Union` x 

flattern :: Doc -> Doc 
flattern (x `Concat` y) = flattern x `Concat` flattern y 
flattern Line = Char ' '
flattern (x `Union` _ ) = flattern x
flattern other = other 

compact :: Doc -> String 
compact x = transform [x]
	where transform [] = "" 
       	      transform (d:ds) = 
	      	case d of 
		  	Empty -> transform ds 
			Char c -> c : transform ds
			Text s -> s ++ transform ds 
			Line ->  '\n' : transform ds
			a `Concat` b -> transform (a:b:ds)
			_ `Union` b -> transform (b:ds)

pretty :: Int -> Doc -> String 
pretty width x = best 0 [x]
	where 
       	   best col (d:ds) =
       		case d of 
		   	Empty -> best col ds
			Char c -> c : best ( col + 1 ) ds 
			Text s -> s ++ best ( col + length s ) ds 
			Line ->  '\n' : best 0 ds 
			a `Concat` b -> best col (a:b:ds)
			a `Union` b -> nicest col ( best col (a:ds) ) ( best col (b:ds) )
	   best _ _ = ""
	   nicest col a b | ( width - least ) `fits` a = a 
	   		  | otherwise = b 
			  where least = min width col 
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs


punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = [] 
punctuate p [d] = [d]
punctuate p (d:dlist) = ( d <> p ) : punctuate p dlist 



fill :: Int -> Doc -> Doc
fill width doc = processNode 0 [doc]
	where processNode col (d:ds) =
       		case d of 
		   	Empty -> processNode col ds 
			Char c -> Char c <> processNode (col + 1) ds 
			Text s -> Text s <> processNode (col + length s) ds 
			Line -> spaceOut col <> Line <> processNode 0 ds 
			a `Concat` b -> processNode col (a:b:ds)
			x `Union` y ->  processNode col (x:ds) `Union` processNode col (y:ds)
	      processNode col [] = spaceOut col
	      spaceOut col = Text( replicate (width-col) ' ' )


otherfill :: Int -> Doc -> Doc
otherfill width x = x <> text ( replicate len ' ' )
	where len = width - length ( compact x )


nest :: Int -> Doc -> Doc
nest indentLevel doc = processNode 0 [0] [doc]
	where processNode col nestStack (d:ds) = 
       		case d of 
			Empty -> Empty <> processNode col nestStack ds 
			Char c -> Char c <> processNode (col+1) newNestStack ds 
				where newNestStack = case c of 
			   		c | c == '{' || c == '[' -> (col + 1 + indentLevel) : nestStack 
			      		c | c == '}' || c == ']' -> tail nestStack 
					otherwise 		 -> nestStack 
			Text s -> Text s <> processNode (col+length s) nestStack ds
			Line -> Line <> indent (head nestStack ) <> processNode 0 nestStack ds 
			a `Concat` b -> processNode col nestStack (a:b:ds)
			x `Union`  y -> processNode col nestStack (x:ds) `Union` processNode col nestStack (y:ds)
	      processNode col newNestStack [] = Empty 
	      indent numberSpace = Text ( replicate numberSpace ' ' ) 

