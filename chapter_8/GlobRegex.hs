-- file: GlobRegex.hs
-- glob 模式是 shell 所使用的简化了的正则表达式
-- http://en.wikipedia.org/wiki/Glob_%28programming%29
-- 星号（*）匹配零个或多个任意字符
-- 问号（?）只匹配一个任意字符
--
module GlobRegex (
	globToRegex,
	matchesGlob
	) where  

import Text.Regex.Posix ((=~))
import Data.Char( isAlpha , toUpper , toLower) 
globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String  
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs 
globToRegex' ('?':cs ) = "." ++ globToRegex' cs 
globToRegex' ('[':'!':c:cs) = "[^" ++ escape c ++ charClass cs
globToRegex' ('[':c:cs)  = '[' : escape c ++  charClass cs 
globToRegex' ('[':_)  = error "unterminated character class"
-- in order to deal with  "Lazy\*" matches Lazy* only 
-- not change to "Lazy\\.*" match "Lazy\o"
-- so "f\\??.c" matches f?o.c not foo.c or f\\o.s
globToRegex' ('\\':c:cs) = '\\' : c : globToRegex' cs 
globToRegex' ('\\':_ ) = error "trailing backslash"
globToRegex' (c:cs) = escape c ++ globToRegex' cs
-- escape 转译
escape :: Char -> String 
escape c | c `elem` regexChars = '\\' : [c]
	 | otherwise = [c]
       where regexChars = "+()^$.{}]|"

-- escape_with_sensitive
escape_with_sensitive :: Char -> Bool -> String
escape_with_sensitive c sensitive | c `elem` regexChars = '\\' : [c]
			| sensitive = case isAlpha c of 
				   	True -> '[' : toUpper(c) : toLower(c) : "]"
					False -> [c]
			| otherwise = [c]
		where regexChars = "+()^$.{}]|"
charClass :: String -> String 
charClass (']':cs ) = ']' : globToRegex' cs
charClass (c:cs) = escape c ++  charClass cs 
charClass [] = error "unterminated character class"


matchesGlob :: FilePath -> String -> Bool 
matchesGlob name pat = name =~ globToRegex' pat 

