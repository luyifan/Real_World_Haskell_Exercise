-- file : JSONClass.hs
{-# LANGUAGE FlexibleInstances #-}
-- export the type contructor JAry JObj, the deconstructor (fromJAry , fromJObj) ,
-- and construction function jary 
-- but not the data constructor JAry JObj
-- 注意这里的类构造函数和值构造函数同名
module JSONClass (
	JAry  (fromJAry),
	JObj  (fromJObj),
	jary 
) where
import Control.Arrow(second)
data JValue = JString String
	    | JNumber Double
	    | JBool Bool
	    | JNull
	    | JObject (JObj JValue)  -- was [(String,  JValue) ]
	    | JArray (JAry JValue)   -- was [ JValue ] 
	    deriving (Eq,Show,Ord)
type JSONError = String

class JSON a where 
	toJValue :: a -> JValue
	fromJValue :: JValue ->  Either JSONError a 

instance JSON JValue where 
	toJValue = id 
	fromJValue = Right

instance JSON Bool where 
	toJValue = JBool 
	fromJValue (JBool b) = Right b
	fromJValue _ = Left "not a JSON boolean"
-- ghci> (fromJValue (JBool True))::Either JSONError Bool
instance JSON String where 
	toJValue = JString
	fromJValue (JString s) = Right s
	fromJValue _ = Left "not a JSON string"

doubleToJValue :: ( Double -> a ) -> JValue -> Either JSONError a 
doubleToJValue f (JNumber n) = Right ( f n ) 
doubleToJValue _ _ = Left "not a JSON number"
instance JSON Int where 
	toJValue = JNumber . realToFrac 
	fromJValue = doubleToJValue  round 

instance JSON Integer where
	toJValue = JNumber . realToFrac 
	fromJValue = doubleToJValue  round 

instance JSON Double where 
	toJValue = JNumber 
	fromJValue = doubleToJValue id 
-- define a function to apply data constructor 
newtype JAry a = JAry {
		fromJAry :: [a] 
	} deriving (Eq,Show,Ord)

jary :: [a] -> JAry a
jary = JAry


jaryToJValue :: (JSON a) => JAry a -> JValue 
jaryToJValue =  JArray . JAry . map toJValue . fromJAry 

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = 
		whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON array"

mapEithers :: ( a -> Either b c ) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of 
			Left err -> Left err
			Right ys -> case f x of 
			   		Left err -> Left err
					Right y -> Right (y:ys)
mapEithers _ _ = Right []

whenRight :: ( b -> c ) -> Either a b -> Either a c 
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)


instance (JSON a) => JSON (JAry a) where 
	toJValue = jaryToJValue 
	fromJValue = jaryFromJValue 
-- Main> let o= JArray (JAry [JBool True,JBool True])
-- Main> fromJValue o::Either JSONError (JAry Bool)
-- Right (JAry {fromJAry = [True,True]})
-- Main> let o = JAry [JBool True , JString "122"]
-- 想这种目前就不能转化到JAry,这是由JAry的局限决定了必须是同一类型
-- Main> fromJValue  it
--
-- <interactive>:137:13:
--     Couldn't match expected type ‘JValue’
--                     with actual type ‘Either JSONError a0’
--                         In the first argument of ‘fromJValue’, namely
--                         ‘it’
--                             In the expression: fromJValue it
-- Main> fromJValue  it::Either JSONError (JAry Bool)
--
-- <interactive>:138:13:
--     Couldn't match expected type ‘JValue’
--                     with actual type ‘Either JSONError a0’
--                         In the first argument of ‘fromJValue’, namely
--                         ‘it’
--                             In the expression: fromJValue it :: Either
--                             JSONError (JAry Bool)
newtype JObj a = JObj {
		fromJObj :: [(String,a)]
	} deriving (Eq,Show,Ord)

jobjToJValue :: (JSON a) => JObj a -> JValue
jobjToJValue = 	 JObject . JObj . map (second toJValue) . fromJObj 
jobjFromJValue :: (JSON a ) => JValue -> Either JSONError (JObj a)
jobjFromJValue (JObject (JObj o)) =
		whenRight JObj (mapEithers unwrap o)
	
jobjFromJValue _ = Left "not a JSON object"
unwrap (k,v) = whenRight ((,) k) (fromJValue v)

instance (JSON a) => JSON (JObj a)  where
	toJValue = jobjToJValue
	fromJValue = jobjFromJValue

--Main> toJValue  (JObj [("name", "luyifan"), ("age", 22)])
--
-- <interactive>:119:47:
--    No instance for (Num [Char]) arising from the literal ‘22’
--        In the expression: 22
--            In the expression: ("age", 22)
--                In the first argument of ‘JObj’, namely
--                      ‘[("name", "luyifan"), ("age", 22)]’
--Main> toJValue  (JObj [("name", "luyifan"), ("age", "22")])
--JObject (JObj {fromJObj = [("name",JString "luyifan"),("age",JString
--"22")]})
--Main> let o = JObject (JObj [("name",JString "luyifan"),("age",JString "22")])
--Main> o
--JObject (JObj {fromJObj = [("name",JString "luyifan"),("age",JString "22")]})
--Main> fromJValue o
--Left "not a JSON number"
--Main> fromJValue o::Either JSONError (JObj String)
--Right (JObj {fromJObj = [("name","luyifan"),("age","22")]})

-- exercise
-- 1. Load the Control.Arrow module into ghci and find out what the second
-- function does.
-- 2. What is the type of (,)? When you use it in ghci, what does it do?
-- What about (,,)?


-- :type (,)
-- (,) :: a -> b -> (a, b)
-- unwrap (k,v) = whenRight ((,) k) (fromJValue v)   get（k,fromJValue v)
-- if no error happens
-- :type (,,)
-- (,,) :: a -> b -> c -> (a, b, c)
-- 理解Control.Arrow 的second
-- :type second
-- second :: Arrow a => a b c -> a (d, b) (d, c)
-- 其中a 是箭头的实例就是(->)
-- 所以变成了
-- second :: (->) b c -> (->) (d , b ) ( d , c)
-- second :: ( b -> c ) -> (d , b ) -> (d ,c )
-- second 接受一个 b 到 c 的函数，以及一个pair(d,b)
-- 然后对pair的第二个元素进行函数转变，产生新的pair(d,c)
