-- file: Monoid.hs
-- 在抽象代数中，有一类简单的抽象结构被称为幺半群。许多数学结构都是幺半群，因为成为幺半群的要求非常低。
-- 一个结构只要满足两个性质便可称为幺半群：
-- 一个满足结合律的二元操作符。我们称之为 (*)：表达式 a * (b * c) 和 (a* b) * c 结果必须相同。
-- 一个单位元素。我们称之为 e，它必须遵守两条法则：a * e == a 和 e * a == a。
-- Haskell 中幺半群无所不在。Data.Monoid 模块定义了 Monoid 类型类。
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import DList
class Monoid a where 
     mempty :: a 
     mappend :: a -> a -> a 

instance Monoid [a] where 
	mempty = []
	mappend = (++)

instance Monoid (DList a) where 
	mempty = empty 
	mappend = append 

-- 尽管从数学的角度看，整数可以以两种不同的方式作为幺半群，但在 Haskell
-- 里，我们却不能给 Int 写两个不同的 Monoid 实例：
-- 编译器会报告重复实例错误。
-- 如果我们真的需要在同一个类型上实现多个 Monoid 实例，我们可以用 newtype
-- 创建不同的类型来达到目的。
newtype AInt = A { unA :: Int }
	deriving (Eq , Show , Num ) 
newtype MInt = M { unM :: Int }
	deriving (Eq,Show,Num)
instance Monoid AInt where 
	mempty = 0 
	mappend = (+)
instance Monoid MInt where 
	mempty = 1 
	mappend = (*)
--ghci> 2 `mappend` 5 :: MInt
--M {unM = 10}
--ghci> 2 `mappend` 5 :: AInt
--A {unA = 7}

-- Data.Sequence 模块定义了 Seq 容器类型
-- Prelude> import qualified Data.Sequence as Seq
-- Prelude Seq> Seq.empty
-- fromList []
-- Prelude Seq> Seq.singleton 1
-- fromList [1]
-- Prelude Seq> let a = Seq.fromList [1, 2, 3]
-- Prelude Seq> 1 Seq.<| Seq.singleton 2
-- Prelude Seq> import Data.Sequence((><), (<|), (|>))
-- Prelude Seq Data.Sequence> Seq.singleton 1 |> 2
-- 一个帮助记忆 (<|) 和 (|>)
-- 函数的方法是，函数的『箭头』总是指向被添加的元素： (<|) 函数要添加的元素在左边，而 (|>) 函数要添加的元素在右边：
--
--不管是从左边添加元素，还是从右边添加元素，添加操作都可以在常数时间内完成。对两个
--Seq 进行追加（append）操作同样非常廉价，复杂度等同于两个 Seq 中较短的那个
--Seq 的长度的对数。
--
--Prelude Seq Data.Sequence> let left = Seq.fromList [1, 3, 3]
--
--Prelude Seq Data.Sequence> let right = Seq.fromList [7, 1]
--
--Prelude Seq Data.Sequence> left >< right
--fromList [1,3,3,7,1]
--
--如果我们想将 Seq 转换回列表，那么就需要 Data.Foldable 模块的帮助：
--Prelude Seq Data.Sequence> import qualified Data.Foldable as Foldable
--Prelude Seq Data.Sequence Foldable> Foldable.toList (Seq.fromList [1, 2,3])

