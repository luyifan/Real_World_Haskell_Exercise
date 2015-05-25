-- file: PNM.hs
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char ( isSpace )

data Greymap = Greymap {
    greyWidth :: Int ,
    greyHeight :: Int ,
    greyMax :: Int ,
    greyData :: L.ByteString 
} deriving (Eq)

instance Show Greymap where 
	show (Greymap w h m _ ) = "Greymap " ++ show w ++ "x" ++ show h ++ " "  ++ show m 
-- return a single parsed Greymap along with the string that remains after
-- parsing 
parseP5 :: L.ByteString -> Maybe (Greymap , L.ByteString)
