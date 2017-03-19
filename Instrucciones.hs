module Instrucciones where
import qualified Lexer as Lexer
import Stdout as Out
import qualified Grammar 
import Data.Map as M 
import Control.Monad.RWS
import Context as Context
import Control.Exception
import Expresiones
import Prelude as P




anaAnidS :: Out.AnidS -> Context.ConMonad ()

anaAnidS (Bif e [] p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Boolean -> do
            modify(modifyTable popTable)
        Number -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el If Se esperaba una expresión Tipo Boolean y se detectó una expresión Tipo Number")



anaAnidS (Bif e ins p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Boolean -> do
            modify (modifyTable popTable)
            mapM_ anaAnidS ins
        Number -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el If Se esperaba una expresión Tipo Boolean y se detectó una expresión Tipo Number")



anaAnidS (Bifelse e [] [] p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Boolean -> do
            modify(modifyTable popTable)
        Number -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el If Se esperaba una expresión Tipo Boolean y se detectó una expresión Tipo Number")


anaAnidS (Bifelse e ins1 [] p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Boolean -> do
            modify(modifyTable popTable)
            mapM_ anaAnidS ins1
        Number -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el If Se esperaba una expresión Tipo Boolean y se detectó una expresión Tipo Number")

anaAnidS (Bifelse e [] ins2 p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Boolean -> do
            modify(modifyTable popTable)
            mapM_ anaAnidS ins2
        Number -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el If Se esperaba una expresión Tipo Boolean y se detectó una expresión Tipo Number")


anaAnidS (Bifelse e ins1 ins2 p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Boolean -> do
            modify(modifyTable popTable)
            mapM_ anaAnidS ins1
            mapM_ anaAnidS ins2
        Number -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el If Se esperaba una expresión Tipo Boolean y se detectó una expresión Tipo Number")

anaAnidS (Bwith [] []) = do
    modify $ modifyHeight (+1)
    st <- get
    sep <- ask
    tell (Out.showLine sep (h st) ("Alcance _with" ++ (show ((h st) - 1)) ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    modify $ modifyHeight (\x -> x-1)
    return ()

anaAnidS (Bwith [] ins) = do
    modify $ modifyHeight (+1)
    st <- get
    sep <- ask
    tell (Out.showLine sep (h st) ("Alcance _with " ++ (show ((h st) - 1)) ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    mapM_ anaAnidS ins
    modify $ modifyHeight (\x -> x-1)
    return ()

anaAnidS (Bwith dls []) = do
    modify $ modifyHeight (+1)
    st <- get
    sep <- ask
    modify $ modifyTable (pushTable (SymTable M.empty (h st)))
    tell (Out.showLine sep (h st) ("Alcance _with " ++ (show ((h st) - 1)) ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    mapM_ anaDecl dls
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    modify $ modifyHeight (\x -> x-1)
    modify $ modifyTable popTable
    return ()

anaAnidS (Bwith dls ins) = do
    modify $ modifyHeight (+1)
    st <- get
    sep <- ask
    modify $ modifyTable (pushTable (SymTable M.empty (h st)))
    tell (Out.showLine sep (h st) ("Alcance _with " ++ (show ((h st) - 1)) ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    mapM_ anaDecl dls
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    mapM_ anaAnidS ins
    modify $ modifyHeight (\x -> x-1)
    modify $ modifyTable popTable
    return ()

anaAnidS (Bwhile e [] p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Boolean -> do
            modify(modifyTable popTable)
        Number -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el While Se esperaba una expresión Tipo Boolean y se detectó una expresión Tipo Number")

anaAnidS (Bwhile e ins p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Boolean -> do
            modify (modifyTable popTable)
            mapM_ anaAnidS ins
        Number -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el While Se esperaba una expresión Tipo Boolean y se detectó una expresión Tipo Number")

anaAnidS (Bfor (Lexer.Identifier p s) e1 e2 []) = do
    st <- get
    let symT = (SymTable M.empty (h st))
    modify $ modifyTable (pushTable (Context.insertSym symT s Context.Number Context.Nein))

    case Context.stringInExpr s e1 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            st <- get
            put st

    case Context.stringInExpr s e2 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            return ()

    st <- get
    sep <- ask
    modify $ modifyHeight (+1)
    tell (Out.showLine sep (h st) ("Alcance _for " ++ (show (h st)) ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st) +2) (s ++ " : number\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    modify $ modifyHeight (\x -> x-1)
    modify (modifyTable popTable)
    return ()

anaAnidS (Bfor (Lexer.Identifier p s) e1 e2 ins) = do
    st <- get
    let symT = (SymTable M.empty (h st))
    modify $ modifyTable (pushTable (Context.insertSym symT s Context.Number Context.Nein))

    case Context.stringInExpr s e1 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            st <- get
            put st

    case Context.stringInExpr s e2 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            return ()

    st <- get
    sep <- ask
    modify $ modifyHeight (+1)
    tell (Out.showLine sep (h st) ("Alcance _for " ++ (show (h st)) ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st) +2) (s ++ " : number\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    mapM_ anaAnidS ins
    modify $ modifyHeight (\x -> x-1)
    modify (modifyTable popTable)
    return ()

anaAnidS (Bforby (Lexer.Identifier p s) e1 e2 e3 []) = do
    st <- get
    let symT = (SymTable M.empty (h st))
    modify $ modifyTable (pushTable (Context.insertSym symT s Context.Number Context.Nein))

    case Context.stringInExpr s e1 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            st <- get
            put st

    case Context.stringInExpr s e2 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            return ()

    case Context.stringInExpr s e3 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            return ()

    st <- get
    sep <- ask
    modify $ modifyHeight (+1)
    tell (Out.showLine sep (h st) ("Alcance _for " ++ (show (h st)) ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st) +2) (s ++ " : number\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    modify $ modifyHeight (\x -> x-1)
    modify (modifyTable popTable)
    return ()

anaAnidS (Bforby (Lexer.Identifier p s) e1 e2 e3 ins) = do
    st <- get
    let symT = (SymTable M.empty (h st))
    modify $ modifyTable (pushTable (Context.insertSym symT s Context.Number Context.Nein))

    case Context.stringInExpr s e1 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            st <- get
            put st

    case Context.stringInExpr s e2 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            return ()

    case Context.stringInExpr s e3 of
        True -> error "De alguna forma llegue aqui"
        False -> do
            return ()

    st <- get
    sep <- ask
    modify $ modifyHeight (+1)
    tell (Out.showLine sep (h st) ("Alcance _for " ++ (show (h st)) ++ ":\n"))
    tell (Out.showLine sep ((h st)+1) ("Variables:\n"))
    tell (Out.showLine sep ((h st) +2) (s ++ " : number\n"))
    tell (Out.showLine sep ((h st)+1) ("Sub_Alcances:\n"))
    mapM_ anaAnidS ins
    modify $ modifyHeight (\x -> x-1)
    modify (modifyTable popTable)
    return ()

anaAnidS (Brepeat e [] p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Number -> do
            modify(modifyTable popTable)
        Boolean -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el Repeat Se esperaba una expresión Tipo Number y se detectó una expresión Tipo Boolean")



anaAnidS (Brepeat e ins p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Number -> do
            modify (modifyTable popTable)
            mapM_ anaAnidS ins
        Boolean -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el Repeat Se esperaba una expresión Tipo Number y se detectó una expresión Tipo Boolean")


anaAnidS (Asig lt e) = do
    let p = takePos lt
    let s = takeStr lt
    st <- get
    case findSym s (onlySymTable(tablas st)) of
            Nothing -> do 
                throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ". Variable " ++ s ++ " no declarada.")
            Just (Context.FoundSym t v alt )-> do
                anaExpr e
                st <- get
                modify (modifyTable popTable)
                case (tipo $ topTable $ tablas st) == t of
                    True -> do
                        return ()
                    False -> do
                        throw $ Context.ContextError ("Cerca de la siguiente posicion " 
                                                        ++ (Out.printPos p)
                                                        ++ ". Asignacion invalida, expresion de distinto tipo a variable asignada")

anaAnidS (InsFcall f) = do
    anaFuncion f
    st <- get
    case topTable $ tablas st of
        FuncionTable r -> do 
            case r of
                Void -> do
                    modify (modifyTable popTable)
                    return ()
                    
                _ -> do
                    throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                                    ++ (Out.showFuncionPos f)
                                                    ++ ", la cual es un llamado de procedimiento (no retorna nada) en una expresion que esperaba tipo de retorno.")
        _ -> do
            error "Error interno, algo salio mal y no esta la tabla de la funcion"

anaAnidS (Read lt) = do
    let p = takePos lt
    let s = takeStr lt
    st <- get
    case findSym s (onlySymTable(tablas st)) of
            Nothing -> do 
                throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ". Variable " ++ s ++ " no declarada.")
            Just (Context.FoundSym t v _ )-> do return ()

anaAnidS (Write args) = do
    anaExprS args
    return ()

anaAnidS (WriteLn args) = do
    anaExprS args
    return()

anaAnidS (Return e p) = do
    anaExpr e
    st <- get
    modify (modifyTable popTable)
    case (funDecl st) of
        None -> do
            throw $ Context.ContextError ("Cerca de la siguiente posicion " 
                                            ++ (Out.printPos p)
                                            ++ ", Funcion de retorno en programa principal")
        _ -> do

            let fid = Context.id (funDecl st)
            modify $ modifyHandler $ replace fid True
            case findFun fid (funcs st) of
                Just (FunProto t lt _) -> do
                    case t of
                        Context.Void -> do
                            throw $ Context.ContextError ("Cerca de la siguiente posicion " 
                                            ++ (Out.printPos p)
                                            ++ ", se detectó una expresión de retorno en la definición de un procedimiento.")
                        _ -> do
                            case (tipo (head $ tablas st) == t) of
                                True -> do return ()
                                False -> throw $ Context.ContextError ("Cerca de la siguiente posicion " 
                                                    ++ (Out.printPos p)
                                                    ++ ", se esperaba una expresion de tipo " ++ (show t))

                Nothing -> do 
                    error "Error interno, algo salio mal y la función no se encuentra en el mapa"
anaAnidS (EmptyB) = do
    return ()
        