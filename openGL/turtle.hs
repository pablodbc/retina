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

vFlip :: Vector2D -> Vector2D
vFlip a = a <**> (-1)

(<+>) :: Vector2D -> Vector2D -> Vector2D
(Vector2D x1 y1) <+> (Vector2D x2 y2) = Vector2D (x1+x2) (y1+y2)

(<->) :: Vector2D -> Vector2D -> Vector2D
a <-> b = a <+> (vFlip b)

vector2DZero :: Vector2D
vector2DZero = Vector2D 0 0

data Movement = Point Vector2D |
				Line Vector2D Vector2D

data TurtleState = TurtleState {
	curPos :: Vector2D,
	maxPos :: Vector2D, 
	minPos :: Vector2D, 
	angle :: Double, -- Radians from 0.0 to 2 * pi
	movs :: [Movement],
	eye :: Bool
}

turtleStart :: TurtleState
turtleStart = TurtleState vector2DZero vector2DZero vector2DZero 0.0 [(Point vector2DZero)] True

eps :: Double
eps = sin pi

near :: Double -> Double -> Bool
near x y = abs(x - y) <= eps

conversionConstant :: Double
conversionConstant = pi / 180.0

toRadians :: Double -> Double
toRadians x = x * conversionConstant

radAdd :: Double -> Double -> Double
radAdd x y 
	| near a circle = 0
	| a >= circle = a - circle
	| near a 0 = 0
	| a < 0 = a + circle
	| otherwise = a
	where 
		a = x + y
		circle = 2 * pi

home :: TurtleState -> TurtleState
home st = TurtleState vector2DZero (maxPos st) (minPos st) (angle st) (movs st) (eye st)

openEye :: TurtleState -> TurtleState
openEye st = TurtleState (curPos st) (maxPos st) (minPos st) (angle st) (movs st) True

closeEye :: TurtleState -> TurtleState
closeEye st = TurtleState (curPos st) (maxPos st) (minPos st) (angle st) (movs st) False

rotateLeft :: TurtleState -> Double -> TurtleState
rotateLeft st dg = TurtleState (curPos st) (maxPos st) (minPos st) newAngle (movs st) (eye st)
	where
		newAngle = radAdd (angle st) (toRadians dg)

rotateRight :: TurtleState -> Double -> TurtleState
rotateRight st dg = rotateLeft st (-dg)

setPosition :: TurtleState -> Double -> Double -> TurtleState
setPosition st x y = TurtleState newPos newMax newMin (angle st) newMovs (eye st)
	where
		newPos = Vector2D (round x) (round y)
		newMax = max newPos (maxPos st)
		newMin = min newPos (minPos st)
		newMovs = 
			if (eye st) then 
				(Point newPos) : (movs st) 
			else 
				(movs st)

forward :: TurtleState -> Double -> TurtleState
forward st dist = TurtleState newPos newMax newMin (angle st) newMovs (eye st)
	where
		currPos = (curPos st)
		newPos = (Vector2D (round (dist * cos(angle st))) (round (dist * sin(angle st)))) <+> currPos
		newMax = max newPos (maxPos st)
		newMin = min newPos (minPos st)
		newMovs = 
			if (eye st) then 
				(Line currPos newPos) : (movs st) 
			else 
				(movs st)

backward :: TurtleState -> Double -> TurtleState
backward st dist = forward st (-dist)


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