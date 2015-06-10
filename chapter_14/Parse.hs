-- file: Parse.hs
module Parse where  
import Data.Int ( Int64 ) 
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char ( chr , isSpace , isDigit)
import Data.Word ( Word8 )
import Control.Applicative ( (<$>) )
data Greymap = Greymap {
    greyWidth :: Int ,
    greyHeight :: Int ,
    greyMax :: Int ,
    greyData :: L.ByteString 
} deriving (Eq)
data ParseState = ParseState {
	string :: L.ByteString ,
	offset :: Int64 
} deriving ( Show ) 

instance Monad Parse where 
	return = identity 
	(>>=) = (==>)
	fail = bail
simpleParse :: ParseState -> (a,ParseState)
simpleParse = undefined 

betterParse :: ParseState -> Either String (a,ParseState)
betterParse = undefined 

newtype Parse a = Parse {
	runParse :: ParseState -> Either String (a,ParseState)
}
-- a simple parse , the identity parser
-- () 中是一个::ParseState -> Either String (a,ParseState) 的函数
identity :: a -> Parse a 
identity a = Parse (\s -> Right (a,s))
-- runParse 是Parse a的解构器，能够获得对应的函数
parse :: Parse a -> L.ByteString -> Either String a 
parse parser initState 
	= case runParse parser ( ParseState initState 0) of 
		Left err -> Left err 
		Right (result,_) -> Right result
-- 修改data类的成员的值的方法
modifyOffset :: ParseState -> Int64 -> ParseState 
modifyOffset initState newOffset = initState { offset = newOffset }

--uncons :: ByteString -> Maybe (Word8, ByteString) Source
--O(1) Extract the head and tail of a ByteString, returning Nothing if it is empty
parseByte :: Parse Word8 
parseByte = 
	getState ==> \initState ->
		case L.uncons(string initState) of 
			Nothing -> bail "no more input"
			Just (byte,remainder) -> 
				putState newState ==> \_ -> identity byte 
			  where newState = initState { string = remainder ,
						       offset = newOffset }
				newOffset = offset initState + 1 
getState :: Parse ParseState 
getState = Parse (\s -> Right (s,s))

putState :: ParseState -> Parse()
putState s = Parse (\_ -> Right((),s))

bail :: String -> Parse a 
bail err = Parse (\s -> Left ("byte offset " ++ show (offset s) ++ " : " ++ err) )

(==>) :: Parse a -> ( a -> Parse b ) -> Parse b 
firstParser ==> secondParser = Parse chainParse 
	where chainParse initState = 
       		case runParse firstParser initState of 
			Left err -> Left err 
			Right (firstResult,newState) -> runParse (secondParser firstResult) newState


instance Functor Parse where 
	fmap f parser = parser ==> \result -> identity (f result)

w2c :: Word8 -> Char 
w2c = chr . fromIntegral 
parseChar :: Parse Char 
parseChar = w2c <$> parseByte 

peekByte :: Parse (Maybe Word8)
peekByte = ((fmap fst) . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char) 
peekChar = fmap w2c <$> peekByte 

parseWhile :: ( Word8 -> Bool ) -> Parse [ Word8 ]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
		if mp == Just True 
	  	then parseByte ==> \b -> (b:) <$> parseWhile p 
		else identity []

parseWhileVerbose p = 
	peekByte ==> \mc ->
		case mc of 
	     	   Nothing -> identity []
		   Just c | p c -> 
		   		parseByte ==> \b ->
				parseWhileVerbose p ==> \bs ->
					identity (b:bs)
		   	  | otherwise -> identity []

parseWhileWith :: (Word8 -> a ) -> ( a -> Bool ) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f ) 

(==>&) :: Parse a -> Parse b -> Parse b 
p ==>& f = p ==> \_ -> f 
skipSpace :: Parse () 
skipSpace = parseWhileWith w2c isSpace ==>& identity ()
assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err
parseNat :: Parse Int 
parseNat = parseWhileWith w2c isDigit ==> \digits -> 
	if null digits 
	 then bail "no more input"
	 else let n = read digits 
	      in if n < 0 
		 then bail "integer overflow"
		 else identity n  
parseRawPGM = 
	parseWhileWith w2c notWhile ==> \header -> 
		skipSpace ==>&	assert( header == "P5")  "invalid raw header" ==>&
		parseNat ==> \width -> skipSpace ==>&
		parseNat ==> \height -> skipSpace ==>&
		parseNat ==> \maxGrey -> parseByte ==>&
		parseBytes (width*height) ==> \bitmap -> 
		identity (Greymap width height maxGrey bitmap)

    where notWhile = (`notElem` " \t\n\r")

parseBytes :: Int -> Parse L.ByteString 
parseBytes n = 
	getState ==> \st -> 
		let n' = fromIntegral n 
      		    (h,t) = L.splitAt n' (string st)
		    st' = st { offset = offset st + L.length h , string = t }
		in putState st' ==>& 
			assert (L.length h == n' ) "end of input" ==>&
			identity h 

-- TODO List 
-- EXERCISES
-- 1. Write a parser for “plain” PGM files.
-- 2. In our description of “raw” PGM files, we omitted a small detail. If
-- the “maximum gray” value in the header is less than 256, each pixel is
-- represented by a single byte. However, it can range up to 65,535, in
-- which case, each pixel will be represented by 2 bytes, in big-endian
-- order (most significant byte first).
-- Rewrite the raw PGM parser to accommodate both the single- and
-- double-byte pixel formats.
-- 3. Extend your parser so that it can identify a raw or plain PGM file,
-- and then parse the appropriate file type.

