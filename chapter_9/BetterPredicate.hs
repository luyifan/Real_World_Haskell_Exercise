-- file: BetterPredicate.hs 
module BetterPredicate (
	Predicate(..),
	betterFind,
	getFileSize,
	InfoP(..),
	pathP,
	sizeP,
	equalP,
	liftP,
	greaterP,
	lesserP,
	liftP2,
	andP,
	orP,
	constP,
	liftPath,
	(==?),
	(&&?),
	(>?)
) where 
import Control.Monad (filterM) 
import System.Directory ( Permissions(..) , getModificationTime , getPermissions ) 
import Data.Time.Clock (UTCTime(..))
import System.FilePath ( takeExtensions )
import Control.Exception ( bracket , handle )
import System.IO ( IOMode(..) , hClose , hFileSize , openFile ) 

import RecursiveContents ( getRecursiveContents )
-- 
-- FileSize : maybe integer  
type Predicate = FilePath -> Permissions -> Maybe Integer -> UTCTime ->  Bool 


betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check 
	where check name = do
		permission <- getPermissions name
		filesize <- getFileSize name 
		time <- getModificationTime name
		return (p name permission filesize time)

simpleFileSize :: FilePath -> IO Integer 
simpleFileSize path = do 
	h <- openFile path ReadMode 
	fileSize <- hFileSize h 
	hClose h 
	return fileSize 

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle ((\_ -> return Nothing )::IOError-> IO (Maybe Integer)) $ do
	h <- openFile path ReadMode  
	fileSize <- hFileSize h 
	hClose h 
	return (Just fileSize)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing )::IOError-> IO (Maybe Integer))  $ do 
	bracket (openFile path ReadMode ) hClose $ \h -> do
		fileSize <- hFileSize h 
		return (Just fileSize)

myTest :: Predicate 
myTest path _ (Just size) _ = 
	takeExtensions path == ".cpp" && size > 131072
myTest _ _ _ _ = False

type InfoP a = FilePath 
	     -> Permissions 
	     -> Maybe Integer 
	     -> UTCTime 
	     -> a
pathP :: InfoP FilePath 
pathP path _ _ _ = path

sizeP :: InfoP Integer 
sizeP _ _ (Just size) _ = size 
sizeP _ _ Nothing _ = -1 

equalP :: (Eq a) => InfoP a  -> a -> InfoP Bool  
equalP f k = \w x y z -> f w x y z == k

-- equalP is the same as equalP'
equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool 
equalP' f k w x y z = f w x y z == k 

liftP :: (a->b->c) -> InfoP a -> b -> InfoP c 
liftP q f k = \w x y z -> f w x y z `q` k 

greaterP , lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool 
simpleAndP f g = \w x y z -> f w x y z && g w x y z 

liftP2 :: (a->b->c) -> InfoP a -> InfoP b -> InfoP c 
liftP2 q f g = \w x y z -> f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> InfoP a 
constP k _ _ _ _ = k 

liftP' q f k = liftP2 q f (constP k)

liftPath :: (FilePath -> a ) -> InfoP a
liftPath f w  _ _ _ = f w

myTest2 = (liftPath takeExtensions `equalP` ".cpp") `andP` ( sizeP `greaterP` 1310172 ) 

-- 用括号括起来是必要的，如果不用括号括起来的话，那么就是操作符，操作符默认的优先级为infixl 9 
-- 而用括号的话，就是函数
-- 要注意myTest3中括号的运用
(==?) = equalP 
(&&?) = andP
(>?) = greaterP
myTest3 = ( liftPath takeExtensions ==? ".cpp" ) &&? ( sizeP >? 1310172 )

-- 如果要去掉括号的话，需要定义优先级
infix 4 ==?
infixr 3 &&?
infix 4 >?
myTest4 = liftPath takeExtensions ==? ".cpp" &&? sizeP >? 1310172 

