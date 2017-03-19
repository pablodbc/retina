module Programa where
import qualified Lexer as Lexer
import Stdout as Out
import qualified Grammar 
import Data.Map.Lazy as M 
import Control.Monad.RWS
import Context as Context
import Control.Exception
import Expresiones
import Instrucciones
import Prelude as P


anaFunDec :: Out.FunDec -> Context.ConMonad ()
anaFunDec (Out.Proc (Lexer.Identifier p s) params ins) = do
    st <- get
    sep <- ask
    modify $ modifyHeight (+1)
    modify $ modifyHandler (replace s False)
    tell (Out.showLine sep (h st) ("Alcance _" ++ s ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    case Context.findFun s (funcs st) of
        Nothing -> do
            case params of
                [] -> do
                    modify $ insertFunProto s (FunProto Context.Void (getTypeList params) 0)
                _ -> do
                    modify $ insertFunProto s (FunProto Context.Void (getTypeList params) 1)
                    modify $ modifyTable (pushTable (SymTable M.empty (h st)))
                    anaParamLs params

        _ -> do
            throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ " la funcion " ++ s ++ "esta siendo declarada dos veces")
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    case ins of
        [] -> do
            return ()
        _ -> mapM_ anaAnidS ins
    case params of
        [] -> do
            return()
        _ -> do
            modify $ modifyTable popTable
    modify (modifyHeight (\x -> x-1))

anaFunDec (Out.Func idt@(Lexer.Identifier p s) params t ins) = do
    st <- get
    sep <- ask
    modify $ modifyHeight (+1)
    let tp = fromTipo t
    modify $ modifyHandler (replace s False)
    tell (Out.showLine sep (h st) ("Alcance _" ++ s ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    case Context.findFun s (funcs st) of
        Nothing -> do
            case params of
                [] -> do
                    modify $ insertFunProto s (FunProto tp (getTypeList params) 0)
                _ -> do
                    modify $ insertFunProto s (FunProto tp (getTypeList params) 1)
                    modify $ modifyTable (pushTable (SymTable M.empty (h st)))
                    anaParamLs params

        _ -> do
            throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ " la funcion " ++ s ++ "esta siendo declarada dos veces")
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    case ins of
        [] -> do
            return ()
        _ -> do
            mapM_ anaAnidS ins
            st <- get
            case ret $ funDecl st of
                True -> do return()
                False -> do
                    throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ " la funcion " ++ s ++ " esperaba al menos una función de retorno")
    case params of
        [] -> do
            return()
        _ -> do
            modify $ modifyTable popTable
    modify $ modifyHandler (backToNone)
    modify (modifyHeight (\x -> x-1))




anaParamLs :: [Out.ParamL] -> Context.ConMonad ()
anaParamLs [] = do
    return ()

anaParamLs (ParamL t (Lexer.Identifier p s):rest) = do
    st <- get
    let symT = topTable $ tablas st
    case symT of
        (SymTable _ _) -> do
            return ()
        _ -> do
            error "Error interno, algo salio mal y no esta la tabla de la simbolos"
    case findSym s (onlySymTable(tablas st)) of
        Just (FoundSym _ _ h ) -> do
            case h == (height symT) of
                True -> do
                    throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ". Variable " ++ s ++ " declarada dos veces en el mismo alcance.")
                False -> do
                    return ()
        _ -> do 
            return ()
    st <- get
    let symT = topTable $ tablas st
    case symT of
        (SymTable _ _) -> do
            case (fromTipo t) of
                Context.Number -> do
                    sep <- ask
                    tell (Out.showLine sep ((h st) +1) (s ++ " : number\n"))
                Context.Boolean -> do
                    sep <- ask
                    tell (Out.showLine sep ((h st) +1) (s ++ " : boolean\n"))
            modify $ modifyTable popTable
            modify $ modifyTable (pushTable (Context.insertSym symT s (fromTipo t) Context.Nein))
            anaParamLs rest
        _ -> do
            error "Error interno, algo salio mal y no esta la tabla de la simbolos"


anaInit :: Init -> ConMonad ()

anaInit (Program [] []) = do
    modify $ insertFunProto "home" (FunProto Void [] 0)
    modify $ insertFunProto "openeye" (FunProto Void [] 0)
    modify $ insertFunProto "closeeye" (FunProto Void [] 0)
    modify $ insertFunProto "forward" (FunProto Void [Number] 1)
    modify $ insertFunProto "backward" (FunProto Void [Number] 1)
    modify $ insertFunProto "rotatel" (FunProto Void [Number] 1)
    modify $ insertFunProto "rotater" (FunProto Void [Number] 1)
    modify $ insertFunProto "setposition" (FunProto Void [Number,Number] 2)
    modify $ insertFunProto "arc" (FunProto Void [Number, Number] 2)
    st <- get
    sep <- ask
    tell (Out.showLine sep (h st) ("Alcance _program:\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    return $! ()

anaInit (Program [] ins) = do
    modify $ insertFunProto "home" (FunProto Void [] 0)
    modify $ insertFunProto "openeye" (FunProto Void [] 0)
    modify $ insertFunProto "closeeye" (FunProto Void [] 0)
    modify $ insertFunProto "forward" (FunProto Void [Number] 1)
    modify $ insertFunProto "backward" (FunProto Void [Number] 1)
    modify $ insertFunProto "rotatel" (FunProto Void [Number] 1)
    modify $ insertFunProto "rotater" (FunProto Void [Number] 1)
    modify $ insertFunProto "setposition" (FunProto Void [Number,Number] 2)
    modify $ insertFunProto "arc" (FunProto Void [Number, Number] 2)
    modify $ modifyHandler backToNone
    st <- get
    sep <- ask
    modify $ modifyHeight (+1)
    tell (Out.showLine sep (h st) ("Alcance _program:\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    mapM_ anaAnidS ins
    modify $ modifyHeight (\x -> x-1)
    return $! ()

anaInit (Program fs []) = do
    modify $ insertFunProto "home" (FunProto Void [] 0)
    modify $ insertFunProto "openeye" (FunProto Void [] 0)
    modify $ insertFunProto "closeeye" (FunProto Void [] 0)
    modify $ insertFunProto "forward" (FunProto Void [Number] 1)
    modify $ insertFunProto "backward" (FunProto Void [Number] 1)
    modify $ insertFunProto "rotatel" (FunProto Void [Number] 1)
    modify $ insertFunProto "rotater" (FunProto Void [Number] 1)
    modify $ insertFunProto "setposition" (FunProto Void [Number,Number] 2)
    modify $ insertFunProto "arc" (FunProto Void [Number, Number] 2)

    st <- get
    sep <- ask
    mapM_ anaFunDec fs
    modify $ modifyHeight (+1)
    tell (Out.showLine sep (h st) ("Alcance _program:\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    modify $ modifyHeight (\x -> x-1)
    return $! ()

anaInit (Program fs ins) = do
    modify $ insertFunProto "home" (FunProto Void [] 0)
    modify $ insertFunProto "openeye" (FunProto Void [] 0)
    modify $ insertFunProto "closeeye" (FunProto Void [] 0)
    modify $ insertFunProto "forward" (FunProto Void [Number] 1)
    modify $ insertFunProto "backward" (FunProto Void [Number] 1)
    modify $ insertFunProto "rotatel" (FunProto Void [Number] 1)
    modify $ insertFunProto "rotater" (FunProto Void [Number] 1)
    modify $ insertFunProto "setposition" (FunProto Void [Number,Number] 2)
    modify $ insertFunProto "arc" (FunProto Void [Number, Number] 2)

    mapM_ anaFunDec fs
    modify $ modifyHandler backToNone
    st <- get
    sep <- ask
    modify $ modifyHeight (+1)
    tell (Out.showLine sep (h st) ("Alcance _program:\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    mapM_ anaAnidS ins
    modify $ modifyHeight (\x -> x-1)
    return $! ()
