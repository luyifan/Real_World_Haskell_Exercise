-- file: Parse.hs
import Data.Int ( Int64 ) 
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Word ( Word8 )
data ParseState = ParseState {
	string :: L.ByteString ,
	offset :: Int64 
} deriving ( Show ) 

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
bail err = Parse (\s -> Left "byte offset " ++ (offset s) ++ " : " + err )

(==>) :: Parse a -> ( a -> Parse b ) -> Parse b 
firstParser ==> secondParser = Parse chainParse 
	where chainParse initState = 
       		case runParse firstParser initState of 
			Left err -> Left err 
			Right (firstResult,newState) -> runParse (secondParser firstResult) newState
