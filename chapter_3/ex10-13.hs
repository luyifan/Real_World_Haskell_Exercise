import Data.List
import Data.Ord (comparing)
-- file: ex10.hs
-- Consider three two-dimensional points, a, b, and c. If we look at the
-- angle formed by the line segment from a to b and the line segment from
-- b to c, it turns left, turns right, or forms a straight line. Define
-- a Direction data type that lets you represent these possibilities.
data Direction = LEFT 
	       | RIGHT 
	       | STRAIGHT
	       deriving (Show,Eq)
-- Write a function that calculates the turn made by three two-dimensional
-- points and returns a Direction.

data CartesianPoint  = Point { x :: Double , y :: Double }
		     deriving (Show)
directionOf ::  CartesianPoint -> CartesianPoint -> CartesianPoint -> Direction
-- http://en.wikipedia.org/wiki/Graham_scan
-- # Three points are a counter-clockwise turn if ccw > 0, clockwise if
-- # ccw < 0, and collinear if ccw = 0 because ccw is a determinant that
-- # gives twice the signed  area of the triangle formed by p1, p2 and p3.
-- function ccw(p1, p2, p3):
--     return (p2.x - p1.x)*(p3.y - p1.y) - (p2.y - p1.y)*(p3.x - p1.x)
directionOf a b c 
	| sign < 0 = LEFT
	| sign > 0 = RIGHT
	| otherwise = STRAIGHT
	where sign = ( x c - x a ) * ( y b - y a ) -  ( y c - y a ) * ( x b - x a)

a = Point  1.0  1.0
b = Point  1.0  2.0
c = Point  1.0  3.0

directionOfList :: [CartesianPoint] -> [Direction]
directionOfList (a:b:c:xlist) = directionOf a b c : directionOfList ( b:c:xlist)
directionOfList _ = []

-- Exercise 13
-- -- Using the code from the preceding three exercises, implement Graham’s
-- scan
-- -- algorithm for the convex hull of a set of 2D points. You can find
-- good
-- -- description of what a convex hull
-- (http://en.wikipedia.org/wiki/Convex_hull)
-- -- is, and how the Graham scan algorithm
-- -- (http://en.wikipedia.org/wiki/Graham_scan) should work, on Wikipedia
-- -- (http://en.wikipedia.org/)
sortByY xlist = sortBy lowerY xlist
	where lowerY a b = compare ( y a , x a ) ( y b , x b )

pointAngle a b = ( x b - x a ) / ( y b - y a ) 

compareAngles = comparing . pointAngle 

sortByAngle xlist = bottomLeft : sortBy (compareAngles bottomLeft) (tail(sortedXlist))
	where sortedXlist = sortByY xlist
       	      bottomLeft = head (sortedXlist)

grahamScan xlist = scan( sortByAngle xlist)
	where scan (a:b:c:xs) = if directionOf a b c == LEFT 
				    then a : scan (c:xs)
				    else a : scan (b:c:xs)
	      scan [a,b] = [a,b]
	      scan _ = []


