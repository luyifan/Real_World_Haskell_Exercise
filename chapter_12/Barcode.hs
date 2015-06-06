-- file: Barcode.hs
import Data.Array (Array(..),(!),bounds,elems,indices,ixmap,listArray,accum)
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl',group,sort,sortBy,tails)
import Data.Maybe (catMaybes,listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import Data.Function (on) 
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as M
import Parse hiding (Greymap)

type Pixel = Word8 
type RGB = (Pixel,Pixel,Pixel)
type Greymap = Array ( Int , Int ) Pixel 
type Pixmap = Array ( Int , Int ) RGB 
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = (10 - sum products) `mod` 10
	where products = mapEveryOther (*3) (reverse ds)

-- zipWith :: ( a -> b -> c ) -> [a] -> [b] -> [c]
-- ($) :: ( a -> b ) -> a -> b 
-- then same as follow ($) :: (x -> y) -> x -> y 
-- so (x->y) is a , x is b , y is c 
-- so zipWith ($) :: [x -> y ] -> [x] -> [y]
-- the same as follow [ a -> b ] -> [a] -> [b]
-- cycle :: [a] -> [a] 
-- cycle [ a -> b ] 
-- 接受有限的函数串，创造无限循环的函数串
-- 和zipWith结合使用，可以使得长度为后面接的串的长度
mapEveryOther :: ( a -> a ) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle[f,id])

-- Array is better than List when they are accessed randomly
leftOddList =  ["0001101", "0011001", "0010011", "0111101", "0100011","0110001", "0101111", "0111011", "0110111", "0001011"]
rightList = map complement <$> leftOddList 
 	where complement '0' = '1'
      	      complement '1' = '0'
leftEvenList = map reverse rightList 

parityList = ["111111", "110100", "110010", "110001", "101100","100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a 
listToArray xs = listArray (0,l-1) xs 
	where l = length xs

leftOddCodes , leftEvenCodes , rightCodes , parityCodes :: Array Int String
leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList 
parityCodes = listToArray parityList 

--Once an array is constructed, we can use the (!) operator to access its elements by index
-- ghci> let a = listArray (0,14) ['a'..]
-- ghci> a ! 2
-- 'c'
-- ghci> a ! 100
-- *** Exception: Error in array index
--
-- ghci> let a = listArray (-9,5) ['a'..]
-- ghci> a ! (-2)
-- 'h'
--
-- The index type can be any member of the Ix type. This lets us use, for
-- example, Char as the index type.
-- ghci> let a = listArray ('a', 'h') [97..]
-- ghci> a ! 'e'
-- 101
--
-- To create a higher-dimensioned array, we use a tuple of Ix instances as
-- the index type.
--
-- ghci> let a = listArray ((0,0),(3,3)) [0..]
-- ghci> a
-- array ((0,0),(3,3))
-- [((0,0),0),((0,1),1),((0,2),2),((0,3),3),((1,0),4),((1,1),5),((1,2),6),((1,3),7),((2,0),8),((2,1),9),((2,2),10),((2,3),11),((3,0),12),((3,1),13),((3,2),14),((3,3),15)]
-- Arrays and laziness
-- 通过只有我们需要获取array的超过范围的下标的数据的时候，它才会出错
-- 所以是惰性的，不过也有strict 的array

-- | Strict left fold , similar to foldl' on lists
foldA :: Ix k => ( a -> b -> a ) -> a -> Array k b -> a 
foldA f s a = go s (indices a)
  where go s (j:js) = let s' = f s (a ! j) 
		      in s' `seq` go s' js 
        go s _  = s

-- | Strict left fold using the first element of the array as its
-- starting value, simliar to foldl1 on lists
foldA1 :: Ix k => ( a -> a -> a ) -> Array k a -> a 
foldA1 f a = foldA f ( a ! fst ( bounds a ) ) a 

encodeEAN13 :: String -> String 
encodeEAN13 = concat . encodeDigits . map digitToInt 
-- this function computes the check digit; don't pass one in 
encodeDigits :: [ Int ] -> [ String ]
encodeDigits s@(first:rest) = 
		outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
	where (left,right) = splitAt 6 rest 
       	      lefties = zipWith leftEncode (parityCodes ! first) left 
	      righties = map rightEncode ( right ++ [ checkDigit s] ) 

leftEncode :: Char -> Int -> String 
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String 
rightEncode = (rightCodes !)

outerGuard = "101"
centerGuard = "01010"

parseRawPPM :: Parse Pixmap 
parseRawPPM = 
	parseWhileWith w2c ( /= '\n') ==> \header -> skipSpace ==>&
	assert (header == "P6") "invalid raw header" ==>&
	parseNat ==> \width -> skipSpace ==>& 
	parseNat ==> \height -> skipSpace ==>&
	parseNat ==> \maxValue -> skipSpace ==>&
	assert (maxValue == 255) "max value out of spec" ==>&
	parseByte ==>&
	parseTimes ( width * height ) parseRGB ==> \pxs -> identity (listArray ((0,0),(width-1,height-1)) pxs)

parseRGB :: Parse RGB 
parseRGB = parseByte ==> \r ->
	   parseByte ==> \g -> 
	   parseByte ==> \b ->
	   identity (r,g,b)

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p ==> \x -> (x:) <$> parseTimes (n-1) p
-- the same as follow , because <$> is operator
-- parseTimes n p = p ==> ( \x -> ( (x:) <$> parseTimes (n-1) p) )

luminance :: ( Pixel , Pixel , Pixel ) -> Pixel
luminance ( r , g , b ) = round ( r' * 0.3 + g' * 0.59 + b' * 0.11 )
	where [r',g',b'] = map fromIntegral [r,g,b]

pixmapToGreymap :: Pixmap -> Greymap 
pixmapToGreymap = fmap luminance 

data Bit = Zero | One 
	 deriving (Eq , Show ) 

threshold :: ( Ix k , Integral a ) => Double -> Array k a -> Array k Bit
threshold n arr = binary <$> arr 
	where binary i | i < pivot = Zero 
       		       | otherwise = One 
	      pivot = round $ least + ( greatest - least ) * n 
	      least = fromIntegral $ choose (<) arr 
	      greatest = fromIntegral $ choose (>) arr 
	      choose f = foldA1 $ \x y -> if f x y then x else y


type Run = Int  
type RunLength a = [(Run,a)]

runLength :: Eq a => [a] -> RunLength a 
runLength = map rle . group 
 	where rle xs = (length xs , head xs)
runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength

type Score = Ratio Int 
scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs 
   where divide d = fromIntegral d / divisor 
	 divisor = fromIntegral (sum xs)
-- A more compact alternative that "knows" we're using Ratio Int
-- scaleToOne xs = map (% sum xs) xs

type ScoreTable = [[ Score ]]
type Digit = Word8
-- "SRL" means "scaled run length".
asSRL :: [ String ] -> ScoreTable 
asSRL = map ( scaleToOne . runLengths )

leftOddSRL = asSRL leftOddList 
leftEvenSRL = asSRL leftEvenList 
rightSRL = asSRL rightList 
paritySRL = asSRL parityList 

distance :: [Score] -> [Score] -> Score 
distance a b = sum . map abs $ zipWith (-) a b
-- The new notation that we introduced in the previous example is an
-- example of a list comprehension, which creates a list from one or more
-- other lists.
-- ghci> [ (a,b) | a <- [1,2], b <- "abc" ]
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]
--
bestScores :: ScoreTable -> [Run] -> [(Score,Digit)]
bestScores srl ps = take 3 . sort $ scores 
	where scores = zip [distance d (scaleToOne ps) | d <- srl] digits 
       	      digits = [ 0..9]
-- In addition to generators, we can also specify guards on the right of
-- a list comprehension. A guard is a Bool expression. If it evaluates to
-- False, that element is skipped over. 
-- ghci> [ (a,b) | a <- [1..6], b <- [5..7], even (a + b ^ 2) ]
-- [(1,5),(1,7),(2,6),(3,5),(3,7),(4,6),(5,5),(5,7),(6,6)]
--
-- We can also bind local variables using a let expression. 
-- ghci> let vowel = (`elem` "aeiou")
-- ghci> [ x | a <- "etaoin", b <- "shrdlu", let x = [a,b], all vowel x ]
-- ["eu","au","ou","iu"]
-- If a pattern match fails in a generator expression, no error occurs.
-- Instead, that list element is skipped. 
-- ghci> [ a | (3,a) <- [(1,'y'),(3,'e'),(5,'p')] ]
-- "e"

data Parity a = Even a | Odd a | None a 
	      deriving (Show)

fromParity :: Parity a -> a 
fromParity (Even a) = a 
fromParity (Odd a ) = a 
fromParity (None a) = a 

parityMap :: ( a -> b ) -> Parity a -> Parity b 
parityMap f ( Even a ) = Even (f a)
parityMap f ( Odd a ) = Odd (f a)
parityMap f ( None a ) = None (f a)

instance Functor Parity where 
	fmap = parityMap

--on :: ( a -> a -> b ) -> ( c -> a ) -> c -> c -> b
--on f g x y = g x `f` g y
compareWithoutParity :: (Ord a) => Parity a -> Parity a -> Ordering
compareWithoutParity = compare `on` fromParity 

bestLeft :: [Run] -> [Parity (Score,Digit)]
bestLeft ps = sortBy compareWithoutParity ((map Odd (bestScores leftOddSRL ps)) ++ (map Even (bestScores leftEvenSRL ps)))

bestRight :: [Run] -> [Parity (Score,Digit)]
bestRight = map None . bestScores rightSRL
{-
data AltParity a = AltEven {fromAltParity :: a}
		 | AltOdd  {fromAltParity :: a}
		 | AltNone {fromAltParity :: a}
		 deriving (Show)
-}

chunkWith :: ([a] -> ([a],[a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h,t) = f xs 
		 in h : chunkWith f t 
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith (splitAt n)

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_,One):_) = []
candidateDigits rle | length rle < 59 =[]
candidateDigits rle | any null match = []
		    | otherwise = map ( map ( fmap snd) ) match 
		where match = map bestLeft left ++ map bestRight right 
		      left = chunksOf 4 . take 24 . drop 3 $ runLengths
		      right = chunksOf 4 . take 24 . drop 32 $ runLengths 
		      runLengths = map fst rle

--Haskell's standard libraries provide two collection types that are
--implemented using balanced trees behind the scenes: Data.Map for
--key/value pairs, and Data.Set for sets of values
--Map k v 
-- It is very important to remember this, as Map's laziness over values is
-- a frequent source of space leaks among coders who are not expecting it.
-- To create an empty map, we use empty. For a map containing one key/value
-- pair, we use singleton.
--
-- ghci>M.empty
-- fromList []
-- ghci>M.singleton "foo" True
-- fromList [("foo",True)]
--
-- ghci> :type M.lookup
-- M.lookup :: (Ord k, Monad m) => k -> M.Map k a -> m a
--return Maybe result , Just or Nothing
--ghci>let m = M.singleton "foo" True :: M.Map String Bool
--ghci>m
--fromList [("foo",True)]
--ghci>M.lookup "foo" m
--Just True
--
--The findWithDefault function takes a value to return if the key isn't in the map
--There exists a (!) operator that performs a lookup and returns the
--unadorned value associated with a key (i.e. not wrapped in Maybe or
--whatever). Unfortunately, it is not a total function: it may call error 
--
--ghci>:type (M.!)
--(M.!) :: Ord k => M.Map k a -> k -> a
--ghci>(M.!)  m "foo"
--True
--ghci>(M.!)  m "bool"
-- *** Exception: Map.!: given key is not an element in the map
--
--To add a key/value pair to the map, the most useful functions are insert
--and insertWith'. The insert function simply inserts a value into the map,
--overwriting any matching value that may already have been present. 
--
--ghci> :type M.insert
--M.insert :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
--ghci> M.insert "quux" 10 m
--fromList [("foo",1),("quux",10)]
--ghci> M.insert "foo" 9999 m
--fromList [("foo",9999)]
--
--The insertWith' function takes a further combining function as its
--argument. If no matching key was present in the map, the new value is
--inserted verbatim. Otherwise, the combining function is called on the new
--and old values, and its result is inserted into the map
--
--ghci> :type M.insertWith'
--M.insertWith' :: (Ord k) => (a -> a -> a) -> k -> a -> M.Map k a -> M.Map
--k a
--ghci> M.insertWith' (+) "zippity" 10 m
--fromList [("foo",1),("zippity",10)]
--ghci> M.insertWith' (+) "foo" 9999 m
--fromList [("foo",10000)]
--
--As the tick at the end of its name suggests, insertWith' evaluates the
--combining function strictly. This allows you to avoid space leaks. While
--there exists a lazy variant insertWith
--
--Finally, there are several efficient functions for performing set-like
--operations on maps. Of these, we'll be using union below. This function
--is “left biased”: if two maps contain the same key, the result will
--contain the value from the left map.
--
--ghci> m `M.union` M.singleton "quux" 1
--fromList [("foo",1),("quux",1)]
--ghci> m `M.union` M.singleton "foo" 0
--fromList [("foo",1)]
--
type Map a = M.Map Digit [a]
type DigitMap = Map Digit 
type ParityMap = Map (Parity Digit)

updateMap :: Parity Digit -- new digit 
	-> Digit -- exist key
	-> [Parity Digit] -- exist digit sequence 
	-> ParityMap  -- map to update
	-> ParityMap 
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)

insertMap :: Digit -> Digit -> [a] -> Map a -> Map a
insertMap key digit val m = val `seq` M.insert key' val m 
	where key' = (key + digit) `mod` 10

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap 
useDigit old new digit = new `M.union` M.foldWithKey ( updateMap digit ) M.empty old 
