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
--

cc2cpp =  mapM  (renameWith (flip replaceExtension ".cpp")) =<< (namesMatching "*.cc")
-- 其实不能那么拆分，其实这里的=<< 是中缀的形式
-- 所以才能解释下面的
-- (=<<) :: Monad m => ( a -> m b ) -> m a -> m b
--(=<<) (namesMatching "*.cc")::Monad m => ([FilePath] -> m b) -> m b
--为什么(=<<)可以不是接受一个函数，而是接受m a
--因为其实需要接受的函数 f应该在(=<<)的左边，即f (=<<) (namesMatching "*.cc") ,所以得到的类型就可以解释啦
--现在要看mapM和=<<的优先级
--通过info mapM和info (=<<)可以看出如下
--ghci> :info map
--map :: (a -> b) -> [a] -> [b] 	-- Defined in ‘GHC.Base’
--ghci> :info (+)
--class Num a where
--  (+) :: a -> a -> a
--    ...
--    -- Defined in ‘GHC.Num’
--   infixl 6 +
-- 可以看出=<<的优先级高，所以正确的括号包围方法
cc2cpp2 = (mapM  (renameWith (flip replaceExtension ".cpp"))) =<< (namesMatching "*.cc")
cc2cpp3 = (=<<) (mapM  (renameWith (flip replaceExtension ".cpp")))(namesMatching "*.cc")
--这样就可以解释啦
--(=<<) (mapM  (renameWith (flip replaceExtension ".cpp"))) :: IO [FilePath] -> [FilePath]
-- 这里主要是优先级和=<<为中缀表示的问题，
-- 其次为什么mapM info没有优先级显示，目前还没有理解

