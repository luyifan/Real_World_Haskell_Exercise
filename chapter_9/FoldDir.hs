-- file: FoldDir.hs
module FoldDir (
	Iterate(..),
	Iterator(..),
	foldTree,
	atMostThreePictures,
	countDirectories 
) where

import ControlledVisit
import System.FilePath((</>))
import System.FilePath.Posix( takeExtensions , takeFileName ) 
import Data.Char ( toLower )
data Iterate seed = Done { unwrap :: seed }
		  | Skip { unwrap :: seed }
		  | Continue { unwrap :: seed }
		  deriving ( Show ) 

type Iterator seed = seed -> Info -> Iterate seed 

foldTree :: Iterator a -> a -> FilePath -> IO a 
foldTree iter seed subpath = do 
	endSeed <- fold seed subpath 
	return (unwrap endSeed)
      where fold seed path = getUsefulContents path >>= walk seed 
	    walk seed (name:names) = do
		    let path' = subpath </> name 
	  	    info <- getInfo path'
		    case iter seed info of 
			done@(Done _) -> return done
			Skip seed' -> walk seed' names
			Continue seed' 
				| isDirectory info -> do
					next <- fold seed' path'
					case next of
	     				   done@(Done _) -> return done 
					   seed'' -> walk (unwrap seed'') names
				| otherwise -> walk seed' names
	    walk seed _ = return (Continue seed)

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info 
	| length paths == 3 
	= Done paths 
	| isDirectory info && takeFileName path == ".svn" 
	= Skip paths
	| extension `elem` [".jpg",".png"]
	= Continue (path:paths)
	| otherwise 
	= Continue paths
   where path = infoPath info
	 extension = map toLower (takeExtensions path)

countDirectories :: Iterator Int 
countDirectories count info = 
		Continue (if isDirectory info then count + 1 else count )


--Todo List 
--EXERCISES
--1. Modify foldTree to allow the caller to change the order of traversal
--of entries in a directory.
--2. The foldTree function performs preorder traversal. Modify it to allow
--the caller to determine the order of traversal.
--3. Write a combinator library that makes it possible to express the kinds
--of iterators that foldTree accepts. Does it make the iterators you write
--any more succinct?
