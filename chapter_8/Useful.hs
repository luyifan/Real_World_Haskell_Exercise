-- file: Useful.hs
import System.FilePath (replaceExtension)
import System.Directory (doesFileExist,renameDirectory,renameFile)
import Glob (namesMatching)

renameWith :: (FilePath -> FilePath) -> FilePath -> IO FilePath
renameWith f path = do
	let path' = f path 
     	rename path path' 
	return path'
-- rename check file or directory
-- don't deal with file or directory does't exist 
rename :: FilePath -> FilePath -> IO() 
rename oldpath newpath = do 
	isFile <- doesFileExist oldpath 
	let f = if isFile then renameFile else renameDirectory 
	f oldpath newpath 

-- changes the file name suffixing convention for C++ source files.
-- replaceExtension :: FilePath -> String -> FilePath 
-- flip :: (a -> b -> c) -> b -> a -> c 
-- flip replaceExtension ".cpp" :: FilePath -> FilePath 
-- renameWith (flip replaceExtension ".cpp") :: FilePath -> IO FilePath 
-- mapM :: Monad m => ( a -> m b ) -> [a] -> m [b]
-- mapM (renameWith (flip replaceExtension ".cpp")) :: [FilePath] -> IO [FilePath]
-- namesMatching "*.cc" :: IO [FilePath]
-- (=<<) :: Monad m => ( a -> m b ) -> m a -> m b 
-- (=<<) (namesMatching "*.cc")::Monad m => ([FilePath] -> m b) -> m b 
-- (=<<) 没有先接受到一个(a -> m b)的函数，而是接受了m a参数也就是IO [FilePath]
-- 所以(=<<) (namesMatching "*.cc") 就如上所示啦
-- 如果我们把后面部分使用一个括号括起来的话(=<< (namesMatching "*.cc"))
-- load会出错
-- seful.hs:32:62:
-- Couldn't match expected type ‘[FilePath]’
--               with actual type ‘([String] -> IO b0) -> IO b0’
--    In the second argument of ‘mapM’, namely
--      ‘(=<< (namesMatching "*.cc"))’
--
--但是我们去掉括号,就可以正确运行，这怎么解释呢
--
--书上说=<< 把IO [FilePath]
--传递到左边，变成[FilePath]不过通过测试感觉不对，不然的话。加括号应该不会出错的
cc2cpp =  mapM  (renameWith (flip replaceExtension ".cpp")) =<< (namesMatching "*.cc")

