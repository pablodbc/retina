{-# LANGUAGE DeriveDataTypeable #-}
module Run where
import qualified Lexer
import qualified Stdout as Out
import qualified Grammar 
import qualified Data.Map.Lazy as M 
import qualified turtle as Tr
import qualified circle as C
import Control.Monad.State.Strict
import Prelude as P
import Control.Exception as E
import Data.Typeable as T
import Data.List

data RuntimeError = RuntimeError String
    deriving (T.Typeable)

instance E.Exception RuntimeError
instance Show RuntimeError where
    show (RuntimeError s) = "Error durante Corrida: " ++ s

type Mutable = Bool

data CompType = Dynamic | Constant deriving (Eq,Show,Ord)

data Type     = Boolean | Number | Void deriving (Eq,Show,Ord)

data ValCalc  = CBoolean Bool | CNumber Double | Nein deriving (Eq,Show,Ord)


data Tabla    = SymTable {mapa :: M.Map String (ValCalc,Mutable), height :: Int} deriving (Eq,Show,Ord)

data FunProto = FunProto {args :: [String], instrucciones :: [AnidS]} deriving (Eq,Show,Ord)


data FunHandler = FunHandler {Maybe ValCalc :: retVal} deriving (Eq,Show,Ord)

data State = State {funcs :: M.Map String FunProto, tablas :: [Tabla], curFun :: FunHandler, h :: Int, ts :: turtleState} deriving (Eq,Show,Ord)



data FoundSym = FoundSym {valor :: ValCalc, altura :: Int, mutable :: Mutable} deriving (Eq,Show,Ord)


-- Monad que usaremos para hacer estas cosas. El primer tipo es arbitrario (Reader maneja el separador)
type RunMonad = StateT State IO

initialState = State M.empty [] None 0

-- ValCalc Modifiers
modifyBoolValCalc :: (Bool -> Bool) -> ValCalc -> ValCalc
modifyBoolValCalc f (CBoolean b) = CBoolean (f b)

operateBoolValCalc :: (Bool -> Bool -> Bool) -> ValCalc -> ValCalc -> ValCalc
operateBoolValCalc f (CBoolean a) (CBoolean b) = CBoolean (f a b)

modifyDoubleValCalc :: (Double -> Double) -> ValCalc -> ValCalc
modifyDoubleValCalc f (CNumber n) = CNumber (f n)

operateDoubleValCalc :: (Double -> Double -> Double) -> ValCalc -> ValCalc -> ValCalc
operateDoubleValCalc f (CNumber n1) (CNumber n2) = CNumber (f n1 n2)

-- Number Handlers
numberConversionHandler :: (RealFrac a, Integral b) => a -> b
numberConversionHandler = floor

applyIntegerFun ::(Integral a, RealFrac b) => (a -> a -> a) -> b -> b -> b
applyIntegerFun f x y = fromIntegral(f (numberConversionHandler x) (numberConversionHandler y))


-- Comparison Handler
comparisonFunNum :: (Eq a, Ord a, RealFrac a) => (a -> a -> Bool) -> a -> a -> ValCalc
comparisonFunNum f x y = CBoolean (f x y)

comparisonFunBool :: (Bool -> Bool -> Bool) -> Bool -> Bool, ts :: turtleState -> ValCalc
comparisonFunBool f x y = CBoolean (f x y)

fromCNumber :: ValCalc -> Double
fromCNumber (CNumber n) = n

fromCBoolean :: ValCalc -> Bool
fromCNumber (CBoolean b) = b





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

modifyTable :: ([Tabla] -> [Tabla]) -> State -> State
modifyTable f (State fs t fd h) = State fs (f t) fd h

modifyHeight :: (Int -> Int) -> State -> State
modifyHeight f (State fs t fd h) = State fs t fd (f h)

-- Handler de Simbolos

isSymTable :: Tabla -> Bool
isSymTable (SymTable _ _) = True
isSymTable _ = False

onlySymTable :: [Tabla] -> [Tabla]
onlySymTable = P.filter (isSymTable)


findSym :: String -> [Tabla] -> Maybe FoundSym

findSym _ [] = Nothing
findSym s (x:xs) = case r of Nothing -> findSym s xs
                             Just (v,b) -> return(FoundSym v (height x) b)
                            where r = M.lookup s (mapa x)

type Pos = Int
findSymTable :: String -> [Tabla] -> Maybe (Tabla,Pos)
findSymTable s [] _ = Nothing
findSymTable s (x:xs) n = case r of 
    Nothing -> findSymTable s xs (n+1)
    Just (v,b) -> return((x,n))
    where r = M.lookup s (mapa x)


insertSym :: String -> ValCalc -> Bool -> Tabla -> Tabla
insertSym s v b  (SymTable m h) = SymTable (M.insert s (v,b) m) h



-- Para encontrar una funcion con un string creo que sería un simple lookup, lo dejaré aquí

findFun :: String -> M.Map String FunProto -> Maybe FunProto
findFun s m = M.lookup s m


insertFunProto :: String -> FunProto -> State -> State
insertFunProto s p (State fs t fd h) = State (M.insert s p fs) t fd h

-- Tipo y Type Handlers
getTypeList :: [Out.ParamL] -> [Context.Type]
getTypeList [] = []
getTypeList ((Out.ParamL t _):rest) = (fromTipo t) : getTypeList rest

fromTipo :: Out.Tipo -> Type
fromTipo Out.BooleanT = Boolean
fromTipo Out.NumberT = Number

--Utilidad para modificar un handler
modifyHandler :: (FunHandler -> FunHandler) -> State -> State
modifyHandler f (State fs t fd h) = State fs t (f fd) h

replace :: Maybe ValCalc -> FunHandler -> FunHandler
replace v f = FunHandler v 


-- Utilidad para saber si una variable esta presente en su propia declaracion
stringInFuncion :: String -> Out.Funcion -> Bool 
stringInFuncion s (Out.FuncionSA lt) = False

stringInFuncion s (Out.FuncionCA lt xprs) = foldr (||) False (map (stringInExpr s) xprs)


stringInExpr :: String -> Out.Expr -> Bool
stringInExpr s (Out.Or e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.And e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Eq e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Neq e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Less e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Lesseq e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.More e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Moreq e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Plus e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Minus e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Mult e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Divex e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Modex e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Div e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Mod e1 e2 _) = (stringInExpr s e1) || (stringInExpr s e2)

stringInExpr s (Out.Not e _) = (stringInExpr s e)

stringInExpr s (Out.Uminus e _) = (stringInExpr s e)

stringInExpr s (Out.Identifier (Lexer.Identifier p id)) 
    | (s == id) = throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                                    ++ (Out.printPos p)
                                                    ++ ", se encontro el uso de la variable "++id++" dentro de su propia definicion")
    | otherwise = False

stringInExpr s (Out.Integer n) = False

stringInExpr s (Out.Floating n) = False

stringInExpr s (Out.ExpTrue b) = False

stringInExpr s (Out.ExpFalse b) = False

stringInExpr s (Out.ExpFcall f) = (stringInFuncion s f)

stringInExpr s (Out.Bracket e) = (stringInExpr s e)
