-- file: RecursiveContents.hs
module RecursiveContents (
	getRecursiveContents
	) where

import Control.Monad ( forM ) 
import System.Directory( doesDirectoryExist , getDirectoryContents ) 
import System.FilePath ( (</>) ) 

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
	names <- getDirectoryContents topdir 
	let reallyName = filter ( `notElem` [".",".."] ) names 
     	paths <- forM reallyName $ \name -> do 
		let path = topdir </> name 
      		isDirectory <- doesDirectoryExist path 
		case isDirectory of 
		      True -> getRecursiveContents path 
		      False -> return [path]
	return (concat paths)
