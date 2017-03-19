{-# LANGUAGE DeriveDataTypeable #-}
module Context where
import qualified Lexer
import qualified Stdout as Out
import qualified Grammar 
import qualified Data.Map.Lazy as M 
import Control.Monad.RWS
import Prelude as P
import Control.Exception as E
import Data.Typeable as T
import Data.List

data ContextError = ContextError String
    deriving (T.Typeable)

instance E.Exception ContextError
instance Show ContextError where
    show (ContextError s) = "Error de Contexto: " ++ s

data CompType = Dynamic | Constant deriving (Eq,Show,Ord)

data Type     = Boolean | Number | Void deriving (Eq,Show,Ord)

data ValCalc  = CBoolean Bool | CNumber Double | Nein deriving (Eq,Show,Ord)


data Tabla    = ExprTable {tipo :: Type, compType :: CompType, val :: ValCalc} |
                FuncionTable {rtype :: Type}                                   |
                SymTable {mapa :: M.Map String (Type,ValCalc), height :: Int}
                deriving (Eq,Show,Ord)

data FunProto = FunProto {retype :: Type, args :: [Type], size :: Int} deriving (Eq,Show,Ord)

data FunHandler = FunHandler {id :: String, ret :: Bool} | None deriving (Eq,Show,Ord)

data State = State {funcs :: M.Map String FunProto, tablas :: [Tabla], funDecl :: FunHandler, h :: Int} deriving (Eq,Show,Ord)

data FoundSym = FoundSym Type ValCalc Int deriving (Eq,Show,Ord)

-- Monad que usaremos para hacer estas cosas. El primer tipo es arbitrario (Reader maneja el separador)
type ConMonad = RWS String String State

initialState = State M.empty [] None 0

-- ValCalc Modifiers
modifyBoolValCalc :: (Bool -> Bool) -> ValCalc -> ValCalc
modifyBoolValCalc f (CBoolean b) = CBoolean (f b)

modifyDoubleValCalc :: (Double -> Double) -> ValCalc -> ValCalc
modifyDoubleValCalc f (CNumber n) = CNumber (f n)

-- Number Handlers
numberConversionHandler :: (RealFrac a, Integral b) => a -> b
numberConversionHandler = floor

applyIntegerFun ::(Integral a, RealFrac b) => (a -> a -> a) -> b -> b -> b
applyIntegerFun f x y = fromIntegral(f (numberConversionHandler x) (numberConversionHandler y))

modex :: RealFrac a => a -> a -> a
modex x y = x - (y * (fromIntegral $ truncate (x/y)))


-- Comparison Handler
comparisonFunNum :: (Eq a, Ord a, RealFrac a) => (a -> a -> Bool) -> a -> a -> ValCalc
comparisonFunNum f x y = CBoolean (f x y)

comparisonFunBool :: (Bool -> Bool -> Bool) -> Bool -> Bool -> ValCalc
comparisonFunBool f x y = CBoolean (f x y)





-- State Handlers
pushTable :: Tabla -> [Tabla] -> [Tabla]
pushTable tabla tablas = tabla:tablas

popTable :: [Tabla] -> [Tabla]
popTable (_:tablas) = tablas

topTable :: [Tabla] -> Tabla
topTable [] = error "hola"
topTable (tabla:tablas) = tabla

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
                             Just (t,v) -> return(FoundSym t v (height x))
                            where r = M.lookup s (mapa x)


insertSym :: Tabla -> String -> Type -> ValCalc -> Tabla
insertSym (SymTable m h) s t v = SymTable (M.insert s (t,v) m) h



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

replace :: String -> Bool -> FunHandler -> FunHandler
replace s b _ = FunHandler s b

backToNone :: FunHandler -> FunHandler
backToNone = (\_ -> None)


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
