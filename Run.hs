{-# LANGUAGE DeriveDataTypeable #-}
module Run where
import qualified Lexer
import qualified Stdout as Out
import qualified Grammar 
import qualified Data.Map.Lazy as M 
import qualified Turtle as Tr
import qualified Circle as C
import Prelude as P
import Control.Exception as E
import Data.Typeable as T
import Data.List
import Control.Monad.State.Strict


data RuntimeError = RuntimeError String
    deriving (T.Typeable)

instance E.Exception RuntimeError
instance Show RuntimeError where
    show (RuntimeError s) = "Error durante Corrida: " ++ s

type Mutable = Bool

data CompType = Dynamic | Constant deriving (Eq,Show,Ord)

data Type     = Boolean | Number | Void deriving (Eq,Show,Ord)

data ValCalc  = CBoolean Bool | CNumber Double | Nein deriving (Eq,Show,Ord)


data Tabla    = SymTable {mapa :: M.Map String (ValCalc,Type,Mutable), height :: Int} deriving (Eq,Show,Ord)

data FunProto = FunProto {tipos :: [Type], args :: [String], instrucciones :: [Out.AnidS]} deriving (Eq,Show,Ord)


data FunHandler = FunHandler {retVal :: Maybe ValCalc} deriving (Eq,Show,Ord)

data RState = RState {funcs :: M.Map String FunProto, tablas :: [Tabla], curFun :: FunHandler, h :: Int, ts :: Tr.TurtleState} deriving (Eq,Show,Ord)



data FoundSym = FoundSym {tipo :: Type, valor :: ValCalc, altura :: Int, mutable :: Mutable} deriving (Eq,Show,Ord)


-- Monad que usaremos para hacer estas cosas. El primer tipo es arbitrario (Reader maneja el separador)
type RunMonad = StateT RState IO

initialState = RState M.empty [] (FunHandler Nothing) 0 Tr.turtleStart

-- ValCalc Modifiers
modifyBoolValCalc :: (Bool -> Bool) -> ValCalc -> ValCalc
modifyBoolValCalc f (CBoolean b) = CBoolean (f b)

operateBoolValCalc :: (Bool -> Bool -> Bool) -> ValCalc -> ValCalc -> ValCalc
operateBoolValCalc f (CBoolean a) (CBoolean b) = CBoolean (f a b)

modifyDoubleValCalc :: (Double -> Double) -> ValCalc -> ValCalc
modifyDoubleValCalc f (CNumber n) = CNumber (f n)

operateDoubleValCalc :: (Double -> Double -> Double) -> ValCalc -> ValCalc -> ValCalc
operateDoubleValCalc f (CNumber n1) (CNumber n2) = CNumber (f n1 n2)

modifyDouboolValCalc :: (Double -> Bool) -> ValCalc -> ValCalc
modifyDouboolValCalc f (CNumber n) = CBoolean (f n)

operateDouboolValCalc :: (Double -> Double -> Bool) -> ValCalc -> ValCalc -> ValCalc
operateDouboolValCalc f (CNumber n1) (CNumber n2) = CBoolean (f n1 n2)


-- Number Handlers
numberConversionHandler :: (RealFrac a, Integral b) => a -> b
numberConversionHandler = floor

applyIntegerFun ::(Integral a, RealFrac b) => (a -> a -> a) -> b -> b -> b
applyIntegerFun f x y = fromIntegral(f (numberConversionHandler x) (numberConversionHandler y))


-- Comparison Handler
comparisonFunNum :: (Eq a, Ord a, RealFrac a) => (a -> a -> Bool) -> a -> a -> ValCalc
comparisonFunNum f x y = CBoolean (f x y)

comparisonFunBool :: (Bool -> Bool -> Bool) -> Bool -> Bool -> ValCalc
comparisonFunBool f x y = CBoolean (f x y)

fromCNumber :: ValCalc -> Double
fromCNumber (CNumber n) = n

fromCBoolean :: ValCalc -> Bool
fromCBoolean (CBoolean b) = b





-- State Handlers
pushTable :: Tabla -> [Tabla] -> [Tabla]
pushTable tabla tablas = tabla:tablas

popTable :: [Tabla] -> [Tabla]
popTable (_:tablas) = tablas

topTable :: [Tabla] -> Tabla
topTable [] = error "hola"
topTable (tabla:tablas) = tabla

replaceAt :: Int -> Tabla -> [Tabla] -> [Tabla]
replaceAt n x xs = take n xs ++ [x] ++ drop (n+1) xs

modifyTable :: ([Tabla] -> [Tabla]) -> RState -> RState
modifyTable f (RState fs t fd h tS) = RState fs (f t) fd h tS

modifyHeight :: (Int -> Int) -> RState -> RState
modifyHeight f (RState fs t fd h tS) = RState fs t fd (f h) tS

-- Handler de Simbolos

{--isSymTable :: Tabla -> Bool
isSymTable (SymTable _ _) = True
isSymTable _ = False

onlySymTable :: [Tabla] -> [Tabla]
onlySymTable = P.filter (isSymTable)
--}

findSym :: String -> [Tabla] -> Maybe FoundSym

findSym _ [] = Nothing
findSym s (x:xs) = case r of Nothing -> findSym s xs
                             Just (v,t,b) -> return(FoundSym t v (height x) b)
                            where r = M.lookup s (mapa x)

type Pos = Int
findSymTable :: String -> [Tabla] -> Int -> Maybe (Tabla,Pos)
findSymTable s [] _ = Nothing
findSymTable s (x:xs) n = case r of 
    Nothing -> findSymTable s xs (n+1)
    _ -> return((x,n))
    where r = M.lookup s (mapa x)


insertSym :: String -> ValCalc -> Type -> Bool -> Tabla -> Tabla
insertSym s v t b (SymTable m h) = SymTable (M.insert s (v,t,b) m) h



-- Para encontrar una funcion con un string creo que sería un simple lookup, lo dejaré aquí

findFun :: String -> M.Map String FunProto -> Maybe FunProto
findFun s m = M.lookup s m


insertFunProto :: String -> FunProto -> RState -> RState
insertFunProto s p (RState fs t fd h tS) = RState (M.insert s p fs) t fd h tS

-- Tipo y Type Handlers
getTypeList :: [Out.ParamL] -> [Type]
getTypeList [] = []
getTypeList ((Out.ParamL t _):rest) = (fromTipo t) : getTypeList rest

getStrList :: [Out.ParamL] -> [String]
getStrList [] = []
getStrList ((Out.ParamL _ idt@(Lexer.Identifier p s) ):rest) = s : getStrList rest

fromTipo :: Out.Tipo -> Type
fromTipo Out.BooleanT = Boolean
fromTipo Out.NumberT = Number

--Utilidad para modificar un handler
modifyHandler :: (FunHandler -> FunHandler) -> RState -> RState
modifyHandler f (RState fs t fd h tS) = RState fs t (f fd) h tS

replace :: Maybe ValCalc -> FunHandler -> FunHandler
replace v f = FunHandler v 


-- Funcion para modificar a la tortuga

modifyTurtle :: (Tr.TurtleState -> Tr.TurtleState) -> RState -> RState
modifyTurtle f (RState fs t fd h tS) = RState fs t fd h (f tS)