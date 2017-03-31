module ExpresionesRun where
import qualified Lexer as Lexer
import Stdout as Out
import qualified Grammar 
import Data.Map.Lazy as M
import Data.Fixed as Fx 
import Control.Monad.RWS
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
                    anaExpr e
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
                    anaExpr e
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


anaExprS :: [Out.ExprS] -> Run.RunMonad ()
anaExprS (x:[]) = do
    case x of
        (Out.StringW _) -> do
            return ()
        (Out.ExprW e) -> do
            anaExpr e
            st <- get
            put (modifyTable popTable st)
            return ()
anaExprS (x:xs) = do
    case x of
        (Out.StringW _) -> do
            anaExprS xs
        (Out.ExprW e) -> do
            anaExpr e
            modify (modifyTable popTable)
            anaExprS xs


anaFuncion :: Out.Funcion -> Run.RunMonad ()
anaFuncion (FuncionSA lt) = do
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
            modify(modifyHandler $ backToNone)
            return v
anaFuncion (FuncionCA lt xprs) = do
    let p = takePos lt
    let s = takeStr lt
    st <- get
    let f = (\(Just k) -> k) $ Run.findFun s (funcs st)
    
        anaArgs a xprs p s
        put ( modifyTable (pushTable (Run.FuncionTable t)) st )
        return ()

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
    anaExpr x
    st <- get
    put $ modifyTable popTable st
    let tp = tipo $ topTable $ tablas st
    
    case tp == a of
        True -> anaArgs args xprs p s
        False -> do throw $ Run.RunError ("Cerca de la siguiente posicion" 
                                            ++ (Out.printPos p)
                                            ++ " funcion: " ++ s ++ " presenta un desajuste de tipos")

anaExpr :: Out.Expr -> RunMonad Run.ValCalc
anaExpr (Out.Or e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    return (Run.operateBoolValCalc (||) r1 r2)

anaExpr (Out.And e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    return (Run.operateBoolValCalc (&&) r1 r2)

anaExpr (Out.Eq e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    case r1 of
        (Run.CBoolean b1) -> return (Run.modifyBoolValCalc (== b1) r2)
        (Run.CNumber n1) -> return (Run.modifyDoubleValCalc (== n1) r2)


anaExpr (Out.Neq e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    case r1 of
        (Run.CBoolean b1) -> return (Run.modifyBoolValCalc (/= b1) r2)
        (Run.CNumber n1) -> return (Run.modifyDoubleValCalc (/= n1) r2)

anaExpr (Out.Less e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    return (Run.operateDoubleValCalc (<) r1 r2)

anaExpr (Out.Lesseq e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    return (Run.operateDoubleValCalc (<=) r1 r2)

anaExpr (Out.More e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    return (Run.operateDoubleValCalc (>) r1 r2)

anaExpr (Out.Moreq e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    return (Run.operateDoubleValCalc (>=) r1 r2)

anaExpr (Out.Plus e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    return (Run.operateDoubleValCalc (+) r1 r2)

anaExpr (Out.Minus e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    return (Run.operateDoubleValCalc (-) r1 r2)

anaExpr (Out.Mult e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    return (Run.operateDoubleValCalc (*) r1 r2)

anaExpr (Out.Divex e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    case r2 of
        (Run.CNumber 0) -> do
            throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en Operacion '/', division entre 0")
        (Run.CNumber n2) -> return (Run.operateDoubleValCalc (/) r1 r2)



anaExpr (Out.Modex e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    case r2 of
        (Run.CNumber 0) -> do
            throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en Operacion '%', division entre 0")
        (Run.CNumber n2) -> return (Run.operateDoubleValCalc (Fx.mod') r1 r2)

anaExpr (Out.Div e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    case r2 of
        (Run.CNumber 0) -> do
            throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en Operacion '/', division entre 0")
        (Run.CNumber n2) -> return (Run.operateDoubleValCalc div r1 r2)

anaExpr (Out.Mod e1 e2 p) = do
    r1 <- anaExpr e1
    r2 <- anaExpr e2
    case r2 of
        (Run.CNumber 0) -> do
            throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en Operacion '%', division entre 0")
        (Run.CNumber n2) -> return (Run.operateDoubleValCalc mod r1 r2)


anaExpr (Out.Not e p) = do
    r <- anaExpr e
    return $ modifyBoolValCalc not r

anaExpr (Out.Uminus e p) = do
    r <- anaExpr e
    return $ modifyBoolValCalc ((-1)*) r

anaExpr (Out.Identifier i@(Lexer.Identifier p s)) = do
    st <- get
    let r = (\(Just k) -> k) $ findSym s (onlySymTable(tablas st))
    return $ valor r 

anaExpr (Out.Integer (Lexer.Integer _ s)) = return (Run.CNumber (read s))

anaExpr (Out.Floating (Lexer.Floating _ s)) = return (Run.CNumber (read s))

anaExpr (Out.ExpTrue (Lexer.True' _ s)) = return (Run.CBoolean True)

anaExpr (Out.ExpFalse (Lexer.False' _ s)) = return (Run.CBoolean False)

anaExpr (Out.ExpFcall f) = return (anaFuncion f)

anaExpr (Out.Bracket e) = anaExpr e



