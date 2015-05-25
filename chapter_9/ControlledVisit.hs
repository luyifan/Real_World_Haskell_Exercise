-- file: ControlledVisit.hs
module ControlledVisit (
	Info(..),
	getInfo ,
	traverse , 
	getUsefulContents , 
	isDirectory 
	) where 

import System.Directory ( Permissions(..) , getDirectoryContents ,  getModificationTime , getPermissions ) 
import Data.Time.Clock ( UTCTime(..) ) 
import System.FilePath ( takeExtensions ) 
import Control.Exception ( bracket , handle ) 
import System.IO ( IOMode(..) , hClose , hFileSize , openFile ) 
import System.FilePath ( (</>) ) 
import RecursiveContents ( getRecursiveContents ) 
import Control.Monad ( liftM , forM ) 
data Info = Info {
	infoPath :: FilePath ,
	infoPerms :: Maybe Permissions ,
	infoSize :: Maybe  Integer ,
	infoModTime :: Maybe  UTCTime 
} deriving ( Eq , Ord , Show ) 

-- Just :: a -> Maybe a 
-- liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r 
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing )::IOError -> IO (Maybe a)) ( Just `liftM` act ) 

getInfo :: FilePath -> IO Info
getInfo path = do 
	perms <- maybeIO (getPermissions path )
	size <- maybeIO ( bracket (openFile path ReadMode ) hClose hFileSize )
	modified <- maybeIO ( getModificationTime path ) 
	return (Info path perms size modified )


traverse :: ( [Info] -> [Info] ) -> FilePath -> IO [Info]
traverse order path = do 
	names <- getUsefulContents path 
	contents <- mapM getInfo (path : map (path </>) names )
	liftM concat $ forM (order contents) $ \info -> do 
			if isDirectory info && infoPath info /= path 
	   		then traverse order (infoPath info)
			else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do 
	names <- getDirectoryContents path 
	return (filter ( `notElem` [".",".."] ) names )
-- maybe :: b -> (a -> b) -> Maybe a -> b
isDirectory :: Info  -> Bool 
isDirectory = maybe False  searchable . infoPerms

traverseVerbose order path = do 
	names <- getUsefulContents path 
	let userfulNames = filter (`notElem` [".",".."]) names
     	contents <- mapM getEntryName ("": userfulNames )
	recursiveContents <- mapM recurse (order contents)
	return (concat recursiveContents) 
   where getEntryName name = getInfo (path </> name )
	 isDirectory info = case infoPerms info of 
				Nothing -> False 
				Just perms -> searchable perms
	 recurse info = 
	 	if isDirectory info && infoPath info /= path 
	   	then traverseVerbose order (infoPath info)
		else return [info]


--Todo List 
--1. What should you pass to traverse to traverse a directory tree in
--reverse alphabetic order?
--2. Using id as a control function, traverse id performs a preorder
--traversal of a tree: it returns a parent directory before its children.
--Write a control function that makes traverse perform a postorder
--traversal, in which it returns children before their parent.
--3. Take the predicates and combinators from “Gluing Predicates Together”
--on page 224 and make them work with our new Info type.
--4. Write a wrapper for traverse that lets you control traversal using one
--predicate and filter results using another.￼
