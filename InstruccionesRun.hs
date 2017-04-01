module InstruccionesRun where
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
import Data.List as L
import Data.Maybe as Mb
import ExpresionesRun
import Run

-- Verifica si hay un valor de retorno
verifyReturn :: RunMonad Bool
verifyReturn = do
	st <- get
	case curFun st of
		Nothing -> return False
		_ -> return True

runAnidS :: AnidS -> RunMonad ()

runAnidS (Bif e [] p) = do
	return ()



runAnidS (Bif e ins p) = do
	r <- verifyReturn
	case r of
		False -> do
			st <- get
			b <- runExpr e
			case b of
				CBoolean True -> do
					mapM_ runAnidS ins
				CBoolean False -> return ()
		True -> return ()

runAnidS (Bifelse e [] [] p) = do
	return ()

runAnidS (Bifelse e ins1 [] p) = do
	r <- verifyReturn
	case r of
		False -> do
			st <- get
			b <- runExpr e
			case b of
				CBoolean True -> do
					mapM_ runAnidS ins1
				CBoolean False -> return ()
		True -> return ()


runAnidS (Bifelse e [] ins2 p) = do
	r <- verifyReturn
	case r of
		False -> do
			st <- get
			b <- runExpr e
			case b of
				CBoolean False -> do
					mapM_ runAnidS ins2
				CBoolean True -> return ()
		True -> return ()

runAnidS (Bifelse e ins1 ins2 p) = do
	r <- verifyReturn
	case r of
		False -> do
			st <- get
			b <- runExpr e
			case b of
				CBoolean True -> do
					mapM_ runAnidS ins1
				CBoolean False -> do
					mmapM_ runAnidS ins2
		True -> return ()



runAnidS (Bwith [] []) = do
    return ()

runAnidS (Bwith [] ins) = do
	r <- verifyReturn
		case r of
			False -> do
				modify $ modifyHeight (+1)
				mapM_ runAnidS ins
				modify $ modifyHeight (\x -> x-1)
			True -> return ()

runAnidS (Bwith dls []) = do
	r <- verifyReturn
		case r of
			False -> do
				modify $ modifyHeight (+1)
				st <- get
				modify $ modifyTable (pushTable (SymTable M.empty (h st)))
				mapM_ runDecl dls
				modify $ modifyHeight (\x -> x-1)
				modify $ modifyTable popTable
				return ()

			True -> return ()
    
runAnidS (Bwith dls ins) = do
	r <- verifyReturn
		case r of
			False -> do
				modify $ modifyHeight (+1)
				st <- get
				modify $ modifyTable (pushTable (SymTable M.empty (h st)))
				mapM_ runDecl dls
				mapM_ runAnidS ins
				modify $ modifyHeight (\x -> x-1)
				modify $ modifyTable popTable
				return ()

			True -> return ()

runAnidS (Bwhile e [] p) = do
    r <- verifyReturn
    	case r of
    		False -> do
    			b <- runExpr e
    			case b of
    				CBoolean True -> do
    					runAnidS (Bwhile e [] p)
    				_ -> do
    					return ()
    		True -> return ()

runAnidS (Bwhile e ins p) = do
    r <- verifyReturn
    	case r of
    		False -> do
    			b <- runExpr e
    			case b of
    				CBoolean True -> do
    					mapM_ runAnids ins
    					runAnidS (Bwhile e ins p)
    				_ -> do
    					return ()
    		True -> return ()


runAnidS (Bfor (Lexer.Identifier p s) e1 e2 []) = do
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

runAnidS (Bfor (Lexer.Identifier p s) e1 e2 ins) = do
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
    mapM_ runAnidS ins
    modify $ modifyHeight (\x -> x-1)
    modify (modifyTable popTable)
    return ()

runAnidS (Bforby (Lexer.Identifier p s) e1 e2 e3 []) = do
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

runAnidS (Bforby (Lexer.Identifier p s) e1 e2 e3 ins) = do
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
    mapM_ runAnidS ins
    modify $ modifyHeight (\x -> x-1)
    modify (modifyTable popTable)
    return ()

runAnidS (Brepeat e [] p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Number -> do
            modify(modifyTable popTable)
        Boolean -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el Repeat Se esperaba una expresión Tipo Number y se detectó una expresión Tipo Boolean")



runAnidS (Brepeat e ins p) = do
    anaExpr e
    st <- get
    case (tipo $ topTable $ tablas st) of
        Number -> do
            modify (modifyTable popTable)
            mapM_ runAnidS ins
        Boolean -> throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ", en el Repeat Se esperaba una expresión Tipo Number y se detectó una expresión Tipo Boolean")


runAnidS (Asig lt e) = do
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

runAnidS (InsFcall f) = do
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

runAnidS (Read lt) = do
    let p = takePos lt
    let s = takeStr lt
    st <- get
    case findSym s (onlySymTable(tablas st)) of
            Nothing -> do 
                throw $ Context.ContextError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ". Variable " ++ s ++ " no declarada.")
            Just (Context.FoundSym t v _ )-> do return ()

runAnidS (Write args) = do
    anaExprS args
    return ()

runAnidS (WriteLn args) = do
    anaExprS args
    return()

runAnidS (Return e p) = do
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
runAnidS (EmptyB) = do
    return ()
        
