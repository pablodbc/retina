module Turtle where
import qualified Data.Map.Strict as M 

data Vector2D = Vector2D {x :: Int , y :: Int}
 
instance Show Vector2D where
	show (Vector2D x y) = "(" ++ show (x) ++ ", " ++ show (y) ++ ")"

instance Eq Vector2D where
	(Vector2D x1 y1) == (Vector2D x2 y2) = (x1 == x2) && (y1 == y2)
	a /= b = not (a == b)

instance Ord Vector2D where
	(Vector2D x1 y1) <= (Vector2D x2 y2) = (x1 < x2) || (x1 == x2 && y1 <= y2)


(<**>) :: (Num a, RealFrac a) => Vector2D -> a -> Vector2D
(Vector2D x y) <**> n = Vector2D (round (fromIntegral(x)*n)) (round (fromIntegral(y)*n))

(<+>) :: Vector2D -> Vector2D -> Vector2D
(Vector2D x1 y1) <+> (Vector2D x2 y2) = Vector2D (x1+x2) (y1+y2)


vector2DZero = Vector2D 0 0

data Movement = Point Vector2D |
				Line Vector2D Vector2D

data TurtleState = TurtleState {
	curPos :: Vector2D,
	maxPos :: Vector2D, 
	minPos :: Vector2D, 
	angle :: Double, -- Radians
	movs :: [Movement],
	eye :: Bool
}


turtleStart = TurtleState vector2DZero vector2DZero vector2DZero 0.0 [(Point vector2DZero)] True




-- data TurtleMap = TurtleMap {
-- 					curPos :: Vector2D,
-- 					maxPos :: Vector2D, 
-- 					minPos :: Vector2D, 
-- 					angle :: Double, 
-- 					worldSpace :: M.Map (Int, Int) Bool, 
-- 					eye :: Bool
-- 				}

-- turtleStart = TurtleMap vector2DZero vector2DZero vector2DZero 0.0 M.empty True

main :: IO ()
main = do
  return ()