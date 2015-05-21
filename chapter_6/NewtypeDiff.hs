-- file: NewtypeDiff.hs

data TwoFields = TwoFields Int Int 

-- 只能一个值构造器一个字段
newtype Okay = ExactlyOne Int
-- 可以使用类型变量 
newtype Param a b = Param (Either a b)
	deriving (Eq , Show )
--Main> Param (Left "b")
--Param (Left "b")
--Main> Param (Left 1)
--Param (Left 1)
--Main> Param (Left 1)  == Param (Left "b")
--    No instance for (Num [Char]) arising from the literal ‘1’
--        In the first argument of ‘Left’, namely ‘1’
--            In the first argument of ‘Param’, namely ‘(Left 1)’
--                In the first argument of ‘(==)’, namely ‘Param (Left 1)’
--Main> Param (Left 1)  == Param (Left 2)
--False
--Main> Param (Left 1)  == Param (Left 1)
--True
-- 可以使用记录语法
newtype Record = Record {
		getInt :: Int	 
	}
	deriving (Show,Eq)
--Main> Record 1
--Record {getInt = 1}
-- 不可以没有字段
-- newtype TooFew = TooFew 
-- 不可以多于一个字段
-- newtype TooManyFields = Fields Int Int
-- 不可以多个值构造器
--newtype TooMayCtors = Bad Int
--		    | Worse Int
