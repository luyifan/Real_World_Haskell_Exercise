-- file: Glob.hs
module Glob(
	namesMatching
	) where
import System.Directory (doesDirectoryExist, doesFileExist,
			getCurrentDirectory, getDirectoryContents )
import System.FilePath (dropTrailingPathSeparator , splitFileName , (</>) )
import System.FilePath.Windows ( isPathSeparator )
import Control.Exception ( handle ) 
import Control.Monad ( forM ) 
import GlobRegex ( matchesGlob ) 
import Data.Char ( toUpper ) 
-- 这里没有考虑转义符号
isPattern :: String -> Bool 
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat 
	| not (isPattern pat) = do 
		exist <- doesNameExist pat 
		return (if exist then [pat] else [] )
	| otherwise = do 
		case splitFileName pat of 
			("./",baseName) -> do 
		       		curDir <- getCurrentDirectory 
				listMatches curDir baseName 
			(dirName,baseName) -> do
				dirs <- case (isPattern dirName) of 
				       		True -> namesMatching (dropTrailingPathSeparator dirName)
						False -> return [dirName]
				let listDir = case (isPattern baseName) of 
						True -> listMatches 
						False -> listPlain
				pathNames <- forM dirs $ \dir -> do
						baseNames <- listDir dir baseName 
						return (map (dir </>) baseNames )
				return (concat pathNames)
						
-- maybe file or directory
doesNameExist :: String -> IO Bool
doesNameExist name = do 
	fileExists <- doesFileExist name 
	directoryExist <- doesDirectoryExist name 
	return (fileExists || directoryExist )

listMatches :: FilePath -> String -> IO [String] 
listMatches dirName pat = do 
	dirName' <- case null dirName of 
			True -> getCurrentDirectory 
			False -> return dirName
	handle ((const (return []))::IOError -> IO[String]) $ do
		names <- getDirectoryContents dirName'  
		let names' =  case isHidden pat of 
				     True -> filter isHidden names 
				     False -> filter (not . isHidden) names
		case (isPathSeparator '\\') of 
			False -> return (filter ( `matchesGlob` pat ) names')
			True -> return (filter ( `matchesGlobWindows` pat ) names' ) 
     where isHidden ('.':_) = True 
	   isHidden _ = False 
	   matchesGlobWindows name patt = (upperString name) `matchesGlob` (upperString patt)
	   upperString = map toUpper 


listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do 
	exist <- if null baseName  
		 then doesDirectoryExist dirName
		 else doesNameExist ( dirName </> baseName )
	return (if exist then [baseName] else [])

-- If you're on a Unix-like system, look through the documentation for the
-- System.Posix.Files module, and see if you can find a replacement for the
-- doesNameExist function. 
-- ghci >:browse System.Posix.Files
-- System.Posix.Files.fileExist :: FilePath -> IO Bool


-- The * wild card only matches names within a single directory. Many
-- shells have an extended wild card syntax, **, that matches names
-- recursively in all directories. For example, **.c would mean “match
-- a name ending in .c in this directory or any subdirectory at any depth”.
-- Implement matching on ** wildcards.
-- 
-- Need todo 

