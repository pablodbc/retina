module ExpresionesRun where
import qualified Lexer as Lexer
import Stdout as Out
import qualified Grammar 
import Data.Map.Strict as M
import Data.Fixed as Fx
import Data.Maybe as Mb 
import Run as Run
import InstruccionesRun
import Turtle
import Control.Exception
import Control.Monad.State.Strict


runDecl :: Out.Decl -> Run.RunMonad ()
runDecl (Inicializacion t (Lexer.Identifier p s) e) = do
    v <- runExpr e
    st <- get
    let symT = topTable $ tablas st
    modify $ popTable $ tablas
    let symT' = (SymTable (insert s (v,(fromTipo t),True) $ mapa symT) $ height symT)
    modify $ pushTable symT'


runDecl (Declaracion t ids) = do
    case t of
        Out.NumberT -> runID Number ids
        Out.BooleanT -> runID Boolean ids

runDecl EmptyD = do
    return ()


runID :: Run.Type -> [Lexer.Token] -> Run.RunMonad ()
runID t ((Lexer.Identifier p s):[]) = do
    st <- get
    let v = if t == Number then CNumber 0 else CBoolean False
    let symT = topTable $ tablas st
    modify $ popTable $ tablas
    let symT' = (SymTable (insert s (v,t,True) $ mapa symT) $ height symT)
    modify $ pushTable symT'




runID t ((Lexer.Identifier p s):rest) = do
    st <- get
    let v = if t == Number then CNumber 0 else CBoolean False
    let symT = topTable $ tablas st
    modify $ popTable $ tablas
    let symT' = (SymTable (insert s (v,t,True) $ mapa symT) $ height symT)
    modify $ pushTable symT'
    runID t rest


runExprS :: [Out.ExprS] -> Run.RunMonad ()
runExprS (x:[]) = do
    case x of
        (Out.StringW lt) -> do
            liftIO $ putStr $ takeStr lt
        (Out.ExprW e) -> do
            val <- runExpr e
            case val of
                CNumber n -> do
                    liftIO $ putStr $ show n
                CBoolean True -> do
                    liftIO $ putStr "true" 
                CBoolean False -> do
                    liftIO $ putStr "false" 
runExprS (x:xs) = do
    case x of
        (Out.StringW lt) -> do
            liftIO $ putStr $ takeStr lt
            runExprS xs
        (Out.ExprW e) -> do
            val <- runExpr e
            case val of
                CNumber n -> do
                    liftIO $ putStr $ show n
                    runExprS xs

                CBoolean True -> do
                    liftIO $ putStr "true" 
                    runExprS xs

                CBoolean False -> do
                    liftIO $ putStr "false"
                    runExprS xs

runFuncion :: Out.Funcion -> Run.RunMonad ValCalc
runFuncion (FuncionSA lt) = do
    let p = takePos lt
    let s = takeStr lt
    st <- get
    let f = (\(Just k) -> k) $ Run.findFun s (funcs st)
    modify $ modifyTable (pushTable (SymTable M.empty (h st)))
    mapM_ runAnidS $ instrucciones f
    st <- get
    case retVal $ curFun st of
        Nothing -> do
            throw $ RunError ("Cerca de la siguiente posicion "
                                ++ (Out.printPos p)
                                ++ " Se esperaba un valor de retorno.")
        Just v -> do
            modify $ modifyTable popTable 
            modify $ modifyHandler $ replace Nothing
            return v
runFuncion (FuncionCA lt xprs) = do
    let p = takePos lt
    let s = takeStr lt
    st <- get
    let f = (\(Just k) -> k) $ Run.findFun s (funcs st)
    modify $ modifyTable (pushTable (SymTable M.empty (h st)))
    loadArgs (tipos f) (args f) xprs
    mapM_ runAnidS $ instrucciones f
    st <- get
    case retVal $ curFun st of
        Nothing -> do
            throw $ RunError ("Cerca de la siguiente posicion "
                                ++ (Out.printPos p)
                                ++ " Se esperaba un valor de retorno.")
        Just v -> do
            modify $ modifyTable popTable
            modify $ modifyHandler $ replace Nothing
            return v


loadArgs :: [Type] -> [String] -> [Expr] -> RunMonad ()
loadArgs [] [] [] = return ()
loadArgs (t:ts) (s:ss) (e:exs) = do
    st <- get
    v <- runExpr e
    let symT = topTable $ tablas st
    modify $ popTable $ tablas
    let symT' = (SymTable (insert s (v,t,True) $ mapa symT) $ height symT)
    modify $ pushTable symT'
    loadArgs ts ss exs


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

runExpr (Out.ExpFcall f) = return (runFuncion f)

runExpr (Out.Bracket e) = runExpr e



