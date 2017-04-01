module ExpresionesRun where
import qualified Lexer as Lexer
import Stdout as Out
import qualified Grammar 
import Data.Map.Lazy as M
import Data.Fixed as Fx
import Data.Maybe as Mb 
import Run as Run
import Control.Exception


anaDecl :: Out.Decl -> Run.RunMonad ()
anaDecl (Inicializacion t (Lexer.Identifier p s) e) = do
    st <- get
    let symT = topTable $ tablas st
    case findSym s (onlySymTable(tablas st)) of
        Just (FoundSym _ _ h ) -> do
            case h == (height symT) of
                True -> do
                    throw $ Run.RunError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ". Variable " ++ s ++ " declarada dos veces en el mismo alcance.")
                False -> do
                    put (st)
        _ -> do 
            put (st)

    case Run.stringInExpr s e of
        True -> error "De alguna forma llegue aqui"
        False -> do
            st <- get
            put st
        
    st <- get
    let symT = topTable $ tablas st
    modify $ modifyTable popTable
    st <- get
    case symT of
        (SymTable _ _) -> do
            case t of
                Out.NumberT -> do
                    put $ modifyTable (pushTable (Run.insertSym symT s Run.Number Run.Nein)) st
                    runExpr e
                    st <- get
                    case topTable $ tablas st of
                        Run.ExprTable Run.Boolean _ _ -> do 
                            throw $ Run.RunError ("Cerca de la siguiente posicion" 
                                                            ++ (Out.printPos p)
                                                            ++ " , se declaro Tipo Number pero se inicializo con una expresion Tipo Boolean")
                        Run.ExprTable Run.Number _ _ -> do
                            sep <- ask
                            tell (Out.showLine sep ((h st) +2) (s ++ " : number\n"))
                            put $ modifyTable popTable st
                            return ()
                        _ -> do
                            error "Error interno, algo salio mal y no esta la tabla de la expresion"
                Out.BooleanT -> do
                    put $ modifyTable (pushTable (Run.insertSym symT s Run.Boolean Run.Nein)) st
                    runExpr e
                    st <- get
                    case topTable $ tablas st of
                        Run.ExprTable Run.Number _ _ -> do 
                            throw $ Run.RunError ("Cerca de la siguiente posicion" 
                                                            ++ (Out.printPos p)
                                                            ++ " , se declaro Tipo Boolean pero se inicializo con una expresion Tipo Number")
                        Run.ExprTable Run.Boolean _ _ -> do
                            sep <- ask
                            tell (Out.showLine sep ((h st) +2) (s ++ " : boolean\n"))
                            put $ modifyTable popTable st
                            return ()
                        _ -> do
                            error "Error interno, algo salio mal y no esta la tabla de la expresion"
        _ -> do
            error "Error interno, algo salio mal y no esta la tabla de la simbolos"
    

anaDecl (Declaracion t ids) = do
    case t of
        Out.NumberT -> anaID Run.Number ids
        Out.BooleanT -> anaID Run.Boolean ids

anaDecl EmptyD = do
    return ()


anaID :: Run.Type -> [Lexer.Token] -> Run.RunMonad ()
anaID t ((Lexer.Identifier p s):[]) = do
    st <- get
    let symT = topTable $ tablas st
    case symT of
        (SymTable _ _) -> do
            put st
        _ -> do
            error "Error interno, algo salio mal y no esta la tabla de la simbolos"
    case findSym s (onlySymTable(tablas st)) of
        Just (FoundSym _ _ h ) -> do
            case h == (height symT) of -- ERROR, HAY QUE RE-PENSAR ESTO
                True -> do
                    throw $ Run.RunError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ". Variable " ++ s ++ " declarada dos veces en el mismo alcance.")
                False -> do
                    put (st)
        _ -> do 
            put (st)
    st <- get
    let symT = topTable $ tablas st
    case symT of
        (SymTable _ _) -> do
            case t of
                Run.Number -> do
                    sep <- ask
                    tell (Out.showLine sep ((h st) +2) (s ++ " : number\n"))
                Run.Boolean -> do
                    sep <- ask
                    tell (Out.showLine sep ((h st) +2) (s ++ " : boolean\n"))
            modify $ modifyTable popTable
            modify $ modifyTable (pushTable (Run.insertSym symT s t Run.Nein))
            return ()
        _ -> do
            error "Error interno, algo salio mal y no esta la tabla de la simbolos"

anaID t ((Lexer.Identifier p s):rest) = do
    st <- get
    let symT = topTable $ tablas st
    case symT of
        (SymTable _ _) -> do
            put st
        _ -> do
            error "Error interno, algo salio mal y no esta la tabla de la simbolos"
    case findSym s (onlySymTable(tablas st)) of
        Just (FoundSym _ _ h ) -> do
            case h == (height symT) of
                True -> do
                    throw $ Run.RunError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ ". Variable " ++ s ++ " declarada dos veces en el mismo alcance.")
                False -> do
                    put (st)
        _ -> do 
            put (st)
    st <- get
    let symT = topTable $ tablas st
    case symT of
        (SymTable _ _) -> do
            case t of
                Run.Number -> do
                    sep <- ask
                    tell (Out.showLine sep ((h st) +2) (s ++ " : number\n"))
                Run.Boolean -> do
                    sep <- ask
                    tell (Out.showLine sep ((h st) +2) (s ++ " : boolean\n"))
            modify $  modifyTable popTable
            modify $ modifyTable (pushTable (Run.insertSym symT s t Run.Nein))
            anaID t rest
        _ -> do
            error "Error interno, algo salio mal y no esta la tabla de la simbolos"


runExprS :: [Out.ExprS] -> Run.RunMonad ()
runExprS (x:[]) = do
    case x of
        (Out.StringW _) -> do
            return ()
        (Out.ExprW e) -> do
            runExpr e
            st <- get
            put (modifyTable popTable st)
            return ()
runExprS (x:xs) = do
    case x of
        (Out.StringW _) -> do
            runExprS xs
        (Out.ExprW e) -> do
            runExpr e
            modify (modifyTable popTable)
            runExprS xs


runFuncion :: Out.Funcion -> Run.RunMonad ()
runFuncion (FuncionSA lt) = do
    let p = takePos lt
    let s = takeStr lt
    st <- get
    let f = (\(Just k) -> k) $ Run.findFun s (funcs st)
    mapM_ runInstruccion $ instrucciones f
    st <- get
    case retVal $ curFun st of
        Nothing -> do
            throw $ RunError ("Cerca de la siguiente posicion "
                                ++ (Out.printPos p)
                                ++ " Se esperaba un valor de retorno.")
        Just v -> do 
            return v
runFuncion (FuncionCA lt xprs) = do
    let p = takePos lt
    let s = takeStr lt
    st <- get
    let f = (\(Just k) -> k) $ Run.findFun s (funcs st)
    mapM_ loadArg $ zip (args f) xprs
        anaArgs a xprs p s
        put ( modifyTable (pushTable (Run.FuncionTable t)) st )
        return ()



loadArg :: ((String,Type),Expr) -> RunMonad ()
loadArg ((s,t),e) = do
    st <- get
    let symT = topTable $ tablas st
    modify(popTable $ tablas)


anaArgs :: [Type] -> [Expr] -> Lexer.AlexPosn -> String -> Run.RunMonad ()
anaArgs [] [] _ _= do
    return ()
anaArgs a [] p s = do throw $ Run.RunError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ " funcion: " ++ s ++ " le faltan argumentos")

anaArgs [] x p s = do throw $ Run.RunError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ " funcion: " ++ s ++ " tiene demasiados argumentos")
anaArgs (a:args) (x:xprs) p s = do
    runExpr x
    st <- get
    put $ modifyTable popTable st
    let tp = tipo $ topTable $ tablas st
    
    case tp == a of
        True -> anaArgs args xprs p s
        False -> do throw $ Run.RunError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ " funcion: " ++ s ++ " presenta un desajuste de tipos")

runExpr :: Out.Expr -> RunMonad Run.ValCalc
runExpr (Out.Or e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateBoolValCalc (||) r1 r2)

runExpr (Out.And e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateBoolValCalc (&&) r1 r2)

runExpr (Out.Eq e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    case r1 of
        (Run.CBoolean b1) -> return (Run.modifyBoolValCalc (== b1) r2)
        (Run.CNumber n1) -> return (Run.modifyDoubleValCalc (== n1) r2)


runExpr (Out.Neq e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    case r1 of
        (Run.CBoolean b1) -> return (Run.modifyBoolValCalc (/= b1) r2)
        (Run.CNumber n1) -> return (Run.modifyDoubleValCalc (/= n1) r2)

runExpr (Out.Less e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDoubleValCalc (<) r1 r2)

runExpr (Out.Lesseq e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDoubleValCalc (<=) r1 r2)

runExpr (Out.More e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDoubleValCalc (>) r1 r2)

runExpr (Out.Moreq e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDoubleValCalc (>=) r1 r2)

runExpr (Out.Plus e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDoubleValCalc (+) r1 r2)

runExpr (Out.Minus e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDoubleValCalc (-) r1 r2)

runExpr (Out.Mult e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDoubleValCalc (*) r1 r2)

runExpr (Out.Divex e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    case r2 of
        (Run.CNumber 0) -> do
            throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en Operacion '/', division entre 0")
        (Run.CNumber n2) -> return (Run.operateDoubleValCalc (/) r1 r2)



runExpr (Out.Modex e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    case r2 of
        (Run.CNumber 0) -> do
            throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en Operacion '%', division entre 0")
        (Run.CNumber n2) -> return (Run.operateDoubleValCalc (Fx.mod') r1 r2)

runExpr (Out.Div e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    case r2 of
        (Run.CNumber 0) -> do
            throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en Operacion '/', division entre 0")
        (Run.CNumber n2) -> return (Run.operateDoubleValCalc div r1 r2)

runExpr (Out.Mod e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    case r2 of
        (Run.CNumber 0) -> do
            throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en Operacion '%', division entre 0")
        (Run.CNumber n2) -> return (Run.operateDoubleValCalc mod r1 r2)


runExpr (Out.Not e p) = do
    r <- runExpr e
    return $ modifyBoolValCalc not r

runExpr (Out.Uminus e p) = do
    r <- runExpr e
    return $ modifyBoolValCalc ((-1)*) r

runExpr (Out.Identifier i@(Lexer.Identifier p s)) = do
    st <- get
    let r = fromJust $ findSym s (onlySymTable(tablas st))
    return $ valor r 

runExpr (Out.Integer (Lexer.Integer _ s)) = return (Run.CNumber (read s))

runExpr (Out.Floating (Lexer.Floating _ s)) = return (Run.CNumber (read s))

runExpr (Out.ExpTrue (Lexer.True' _ s)) = return (Run.CBoolean True)

runExpr (Out.ExpFalse (Lexer.False' _ s)) = return (Run.CBoolean False)

runExpr (Out.ExpFcall f) = return (anaFuncion f)

runExpr (Out.Bracket e) = runExpr e



