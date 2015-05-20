-- file: ShowOverlapSolved.hs
import Data.List ( intercalate )
class MyShow a where 
	myShow :: a -> String
	myShowList :: [a] -> String
	-- default implementation
	myShowList xs = "[" ++ ( intercalate "," $ map myShow xs ) ++ "]"

instance MyShow Char where 
	myShow c = "'" ++ [c] ++ "'"
	-- overriding the default
	myShowList xs = "\"" ++ xs ++ "\""

instance MyShow Int where 
	myShow  = show 
	-- use the default myShowList

instance (MyShow a) =>  MyShow [a] where
	myShow = myShowList 
-- *Main Data.Ratio> myShow '1'
-- "'1'"
-- *Main Data.Ratio> myShow "123"
-- "\"123\""
-- *Main Data.Ratio> (myShow (123::Int))
-- "123"
-- 这里主要要解决show [Char] 和 show [Int]是不同的，show "123" -> "\"123\""
-- 而show 123 -> "123"
-- 所有需要采用不同的处理方式，而又不能定义两个类型类实例，所以一个采用默认的，一个采用重载的处理
myPrint :: (MyShow a) => a -> IO ()
myPrint = putStrLn . myShow

main :: IO ()
main = do
		myPrint (1 :: Int)
		myPrint ([1,2,3] :: [Int])
		myPrint 'x'
		myPrint ['x', 'y', 'z']
		myPrint "xyz"
