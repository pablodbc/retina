module Turtle where
import qualified Data.Map.Strict as M
import System.IO 
import Data.Fixed
import qualified Circle as C
-- Math

epsilon :: Double
epsilon = sin pi

near :: Double -> Double -> Bool
near x y = abs(x - y) <= epsilon

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


-- 2D Vector

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

specialMin :: Vector2D -> Vector2D -> Vector2D
specialMin (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (min x1 x2) (min y1 y2)

specialMax :: Vector2D -> Vector2D -> Vector2D
specialMax (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (max x1 x2) (max y1 y2)

vector2DZero :: Vector2D
vector2DZero = Vector2D 0 0

toVector2D :: C.Point -> Vector2D
toVector2D (x, y) = Vector2D x y

betweenAngles :: Vector2D -> Vector2D -> Double -> Double -> Bool
betweenAngles c a s f
	| ((y circlePoint) < 0) = (start <= (angle + (2*pi))) && ((angle + (2*pi)) <= end)
	| otherwise = (start <= angle) && (angle <= end)
	where
		start = min s f
		end = max s f
		circlePoint = a <-> c
		angle = atan2 (fromIntegral(y circlePoint)) (fromIntegral(x circlePoint))

-- Turtle State

data Movement = Point Vector2D |
				Line Vector2D Vector2D
				deriving (Eq,Ord,Show)

data TurtleState = TurtleState {
	curPos :: Vector2D,
	maxPos :: Vector2D, 
	minPos :: Vector2D, 
	angle :: Double, -- Radians from 0.0 to 2 * pi
	movs :: [Movement],
	eye :: Bool
} 	deriving (Eq,Ord,Show)

turtleStart :: TurtleState
turtleStart = TurtleState vector2DZero vector2DZero vector2DZero (pi/2) [(Point vector2DZero)] True


-- Turtle Movement

home :: TurtleState -> TurtleState
home st = TurtleState vector2DZero (maxPos st) (minPos st) (angle st) (movs st) (eye st)

openEye :: TurtleState -> TurtleState
openEye st = TurtleState (curPos st) (maxPos st) (minPos st) (angle st) (movs st) True

closeEye :: TurtleState -> TurtleState
closeEye st = TurtleState (curPos st) (maxPos st) (minPos st) (angle st) (movs st) False

rotateLeft :: Double -> TurtleState -> TurtleState
rotateLeft dg st = TurtleState (curPos st) (maxPos st) (minPos st) newAngle (movs st) (eye st)
	where
		newAngle = radAdd (angle st) (toRadians dg)

rotateRight :: Double -> TurtleState -> TurtleState
rotateRight dg st = rotateLeft (-dg) st

insertPoint :: Vector2D -> TurtleState -> TurtleState
insertPoint newPos st = TurtleState (curPos st) newMax newMin (angle st) newMovs (eye st)
	where
		newMax = 
			if (eye st) then 
				specialMax newPos (maxPos st)
			else
				(maxPos st)
		newMin = 
			if (eye st) then 
				specialMin newPos (minPos st)
			else
				(minPos st)
		newMovs = 
			if (eye st) then 
				(Point newPos) : (movs st) 
			else 
				(movs st)

insertPoints :: [Vector2D] -> TurtleState -> TurtleState
insertPoints [] st = st
insertPoints (x:xs) st = insertPoints xs (insertPoint x st)

setPosition :: Double -> Double -> TurtleState -> TurtleState
setPosition x y st = TurtleState newPos newMax newMin (angle st) newMovs (eye st)
	where
		newPos = Vector2D (round x) (round y)
		newMax = 
			if (eye st) then 
				specialMax newPos (maxPos st)
			else
				(maxPos st)
		newMin = 
			if (eye st) then 
				specialMin newPos (minPos st)
			else
				(minPos st)
		newMovs = 
			if (eye st) then 
				(Point newPos) : (movs st) 
			else 
				(movs st)

forward :: Double -> TurtleState -> TurtleState
forward dist st = TurtleState newPos newMax newMin (angle st) newMovs (eye st)
	where
		currPos = (curPos st)
		newPos = (Vector2D (round (dist * cos(angle st))) (round (dist * sin(angle st)))) <+> currPos
		newMax = 
			if (eye st) then 
				specialMax newPos (maxPos st)
			else
				(maxPos st)
		newMin = 
			if (eye st) then 
				specialMin newPos (minPos st)
			else
				(minPos st)
		newMovs = 
			if (eye st) then 
				if (near dist 0) then
					(Point currPos) : (movs st)
				else
					(Line currPos newPos) : (movs st) 
			else 
				(movs st)

backward :: Double -> TurtleState -> TurtleState
backward dist st = forward (-dist) st

arc :: Double -> Double -> TurtleState -> TurtleState
arc an r st
	| not (eye st) = st
	| otherwise = newSt
	where
		currPos = (curPos st)
		realAn = abs (toRadians an)
		realR = round r
		mid = (realAn/2)
		start = radAdd (angle st) (-mid)
		end = radAdd (angle st) mid
		filterFun 
			| realAn >= 2 * pi  || near realAn (2* pi) = (\_ -> True)
			| near realAn 0 = (\_ -> False)
			| start < end = (\x -> betweenAngles currPos x start end)
			| otherwise = (\x -> (betweenAngles currPos x start (2*pi)) || (betweenAngles currPos x 0 end) )

		points = (map toVector2D (C.generateCirclePoints (x currPos, y currPos) realR))
		pointsF = currPos : (filter filterFun points)
		newSt = insertPoints pointsF st 


-- Graphics

-- Funciones balancedWord y bla tomadas de roguebasin. 
-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)
 
-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
-- See http://www.roguebasin.com/index.php?title=Bresenham%27s_Line_Algorithm#Haskell
bla :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bla (x0, y0) (x1, y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in  walk (balancedWord p q 0) (x0, y0)


linePoints :: Vector2D -> Vector2D -> [(Int, Int)]
linePoints a b
	| a' <= b' = takeWhile (<= b') (bla a' b')
	| otherwise = takeWhile (<= a') (bla b' a')
	where
		a' = (x a, y a)
		b' = (x b, y b)

normWorldSpace :: Vector2D -> Vector2D -> Vector2D
normWorldSpace mini a = a <-> mini

viewSpaceMin :: Vector2D -> Vector2D -> Vector2D
viewSpaceMin mini maxi = normWorldSpace mini (Vector2D (x mini) (y maxi)) 

toViewSpace :: Vector2D -> Vector2D -> Vector2D
toViewSpace (Vector2D x0 y0) (Vector2D x y) = Vector2D (x - x0) (y0 - y)

runMovements :: (Vector2D -> Vector2D) -> M.Map (Int, Int) Bool -> [Movement] -> M.Map (Int, Int) Bool
runMovements _ ma [] = ma
runMovements f ma ((Point a):arr) = runMovements f newMa arr
	where 
		aux = f a
		newMa = M.insert (x aux, y aux) True ma
runMovements f ma ((Line a b):arr) = runMovements f newMa arr
	where 
		aux = linePoints (f a) (f b)
		newMa = foldr (\x -> M.insert x True) ma aux 

showRow ::  M.Map (Int, Int) Bool -> Int -> Int -> String
showRow ma width r = res ++ "\n"
	where
		points = zip (takeWhile (<= width) [0..]) (repeat r)
		row2String :: (Int, Int) -> String
		row2String pos =
			case M.lookup pos ma of
				Nothing -> 		"0 "
				(Just False) -> "0 "
				(Just True) -> 	"1 "
		res = foldr (++) "" (map row2String points) 

showRows :: M.Map (Int, Int) Bool -> Int -> Int -> String
showRows ma width height = (show (width+1)) ++ " " ++ (show (height+1)) ++ "\n" ++ res
	where
		rows = takeWhile (<= height) [0..]
		res = foldr (++) "" (map (showRow ma width) rows) 

showTurtle :: TurtleState -> String
showTurtle st = "P1\n# Made by Christian Oliveros & Pablo Betancourt\n" ++ res
	where
		minView = viewSpaceMin (minPos st) (maxPos st)
		f = (toViewSpace minView) . (normWorldSpace (minPos st))
		viewMap = runMovements f M.empty (movs st)
		width = (x $ f (maxPos st))
		height =(y $ f (minPos st))
		res = showRows viewMap width height

drawTurtleTest :: TurtleState -> IO ()
drawTurtleTest st = writeFile "retina.pbm" (showTurtle st)


drawTurtle :: String -> TurtleState -> IO ()
drawTurtle s st = writeFile s' (showTurtle st)
	where 
		s' = (reverse $ cutByDot $ reverse s) ++ ".pbm" 


cutByDot :: String -> String
cutByDot [] = error "dot not found"
cutByDot ('.':xs) = xs
cutByDot (x:xs) = cutByDot xs