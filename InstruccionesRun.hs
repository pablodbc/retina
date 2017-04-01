module InstruccionesRun where
import qualified Lexer
import Stdout as Out
import qualified Grammar 
import qualified Turtle as Tr
import qualified Circle as C
import Control.Monad.State.Strict
import System.IO
import Data.Map.Strict as M
import Data.Fixed as Fx
import Prelude as P
import Control.Exception as E
import Data.Typeable as T
import Data.List as L
import Data.Maybe as Mb
import Run
import qualified Text.Read as Tx



runDecl :: Out.Decl -> Run.RunMonad ()
runDecl (Inicializacion t (Lexer.Identifier p s) e) = do
    v <- runExpr e
    st <- get
    let symT = topTable $ tablas st
    modify $ modifyTable $ popTable
    let symT' = (SymTable (M.insert s (v,(fromTipo t),True) $ mapa symT) $ height symT)
    modify $ modifyTable $ pushTable symT'


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
    modify $ modifyTable $ popTable
    let symT' = (SymTable (M.insert s (v,t,True) $ mapa symT) $ height symT)
    modify $ modifyTable $ pushTable symT'




runID t ((Lexer.Identifier p s):rest) = do
    st <- get
    let v = if t == Number then CNumber 0 else CBoolean False
    let symT = topTable $ tablas st
    modify $ modifyTable $ popTable
    let symT' = (SymTable (M.insert s (v,t,True) $ mapa symT) $ height symT)
    modify $ modifyTable $ pushTable symT'
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
            throw $ RuntimeError ("Cerca de la siguiente posicion "
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
            throw $ RuntimeError ("Cerca de la siguiente posicion "
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
    modify $ modifyTable $ popTable
    let symT' = (SymTable (M.insert s (v,t,True) $ mapa symT) $ height symT)
    modify $ modifyTable $ pushTable symT'
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
        (Run.CNumber n1) -> return (Run.modifyDouboolValCalc (== n1) r2)


runExpr (Out.Neq e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    case r1 of
        (Run.CBoolean b1) -> return (Run.modifyBoolValCalc (/= b1) r2)
        (Run.CNumber n1) -> return (Run.modifyDouboolValCalc (/= n1) r2)

runExpr (Out.Less e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDouboolValCalc (<) r1 r2)

runExpr (Out.Lesseq e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDouboolValCalc (<=) r1 r2)

runExpr (Out.More e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDouboolValCalc (>) r1 r2)

runExpr (Out.Moreq e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    return (Run.operateDouboolValCalc (>=) r1 r2)

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
        (Run.CNumber n2) -> do
            let n1 = fromCNumber r1
            return (CNumber $ applyIntegerFun div n1 n2)

runExpr (Out.Mod e1 e2 p) = do
    r1 <- runExpr e1
    r2 <- runExpr e2
    case r2 of
        (Run.CNumber 0) -> do
            throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en Operacion '%', division entre 0")
        (Run.CNumber n2) -> do
            let n1 = fromCNumber r1
            return (CNumber $ applyIntegerFun mod n1 n2)


runExpr (Out.Not e p) = do
    r <- runExpr e
    return $ modifyBoolValCalc not r

runExpr (Out.Uminus e p) = do
    r <- runExpr e
    return $ modifyDoubleValCalc ((-1)*) r

runExpr (Out.Identifier i@(Lexer.Identifier p s)) = do
    st <- get
    let r = fromJust $ findSym s (tablas st)
    return $ valor r 

runExpr (Out.Integer (Lexer.Integer _ s)) = return (Run.CNumber (read s))

runExpr (Out.Floating (Lexer.Floating _ s)) = return (Run.CNumber (read s))

runExpr (Out.ExpTrue (Lexer.True' _ s)) = return (Run.CBoolean True)

runExpr (Out.ExpFalse (Lexer.False' _ s)) = return (Run.CBoolean False)

runExpr (Out.ExpFcall f) = do
    g <- runFuncion f
    return (g)

runExpr (Out.Bracket e) = runExpr e






-- Verifica si hay un valor de retorno
verifyReturn :: RunMonad Bool
verifyReturn = do
    st <- get
    case curFun st of
        FunHandler Nothing -> return False
        _ -> return True

-- Utilidad para correr un ciclo despues de insertar la tabla donde esta el simbolo no mutable

runFor :: String -> Integer -> [AnidS] -> RunMonad ()
runFor s end [] = do
    st <- get
    let var = fromJust $ findSym s $ tablas st
    let k = valor var
    case operateDouboolValCalc (<) k (CNumber $ fromIntegral end) of
        CBoolean True -> do
            let newK = modifyDoubleValCalc (+1) k
            let (x,n) = fromJust $ findSymTable s (tablas st) 0
            let x1 = (SymTable (M.insert s (newK,(tipo var),False) $ mapa x) $ height x)
            modify $ modifyTable $ replaceAt n x1
            runFor s end []
        CBoolean False -> return ()

runFor s end ins = do
    st <- get
    let var = fromJust $ findSym s $ tablas st
    let k = valor var
    case operateDouboolValCalc (<) k (CNumber $ fromIntegral end) of
        CBoolean True -> do
            mapM_ runAnidS ins
            let newK = modifyDoubleValCalc (+1) k
            let (x,n) = fromJust $ findSymTable s (tablas st) 0
            let x1 = (SymTable (M.insert s (newK,(tipo var),False) $ mapa x) $ height x)
            modify $ modifyTable $ replaceAt n x1
            runFor s end ins
        CBoolean False -> return ()

runForBy :: String -> Double -> [AnidS] -> Double -> RunMonad ()
runForBy s end [] step = do
    st <- get
    let var = fromJust $ findSym s $ tablas st
    let k = valor var
    case operateDouboolValCalc (<) k (CNumber end) of
        CBoolean True -> do
            let newK = modifyDoubleValCalc (+step) k
            let (x,n) = fromJust $ findSymTable s (tablas st) 0
            let x1 = (SymTable (M.insert s (newK,(tipo var),False) $ mapa x) $ height x)
            modify $ modifyTable $ replaceAt n x1
            runForBy s end [] step
        CBoolean False -> return ()

runForBy s end ins step = do
    st <- get
    let var = fromJust $ findSym s $ tablas st
    let k = valor var
    case operateDouboolValCalc (<) k (CNumber end) of
        CBoolean True -> do
            mapM_ runAnidS ins
            let newK = modifyDoubleValCalc (+step) k
            let (x,n) = fromJust $ findSymTable s (tablas st) 0
            let x1 = (SymTable (M.insert s (newK,(tipo var),False) $ mapa x) $ height x)
            modify $ modifyTable $ replaceAt n x1
            runForBy s end ins step
        CBoolean False -> return ()


runRep :: Int -> [AnidS] -> RunMonad ()
runRep 0 _ = return ()
runRep n [] = runRep (n-1) []
runRep n ins = do
    mapM_ runAnidS ins
    runRep (n-1) ins

runProc :: Funcion -> RunMonad ()
runProc (FuncionSA lt) = do
    let p = takePos lt
    let s = takeStr lt
    case s of
        "home" -> do
            modify $ modifyTurtle Tr.
    st <- get
    let f = (\(Just k) -> k) $ Run.findFun s (funcs st)
    modify $ modifyTable (pushTable (SymTable M.empty (h st)))
    mapM_ runAnidS $ instrucciones f
    modify $ modifyTable popTable 
    modify $ modifyHandler $ replace Nothing

runProc (FuncionCA lt xprs) = do
    let p = takePos lt
    let s = takeStr lt
    st <- get
    let f = (\(Just k) -> k) $ Run.findFun s (funcs st)
    modify $ modifyTable (pushTable (SymTable M.empty (h st)))
    loadArgs (tipos f) (args f) xprs
    mapM_ runAnidS $ instrucciones f
    modify $ modifyTable popTable
    modify $ modifyHandler $ replace Nothing

runAnidS :: Out.AnidS -> RunMonad ()

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
                    mapM_ runAnidS ins2
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
                    mapM_ runAnidS ins
                    runAnidS (Bwhile e ins p)
                _ -> do
                    return ()
        True -> return ()


runAnidS (Bfor (Lexer.Identifier p s) e1 e2 []) = do
    r <- verifyReturn
    case r of
        False -> do
            st <- get
            inicio <- runExpr e1
            final <- runExpr e2
            let start = fromInteger $ floor $ fromCNumber inicio
            let end = fromInteger $ floor $ fromCNumber final
            let symT = (SymTable M.empty (h st))
            modify $ modifyTable (pushTable (Run.insertSym s (CNumber(fromIntegral $ start)) Number False symT))
            modify $ modifyHeight (+1)
            runFor s end []
            modify $ modifyHeight (\x -> x-1)
            modify (modifyTable popTable)
        True -> return ()



runAnidS (Out.Bfor (Lexer.Identifier p s) e1 e2 ins) = do
    r <- verifyReturn
    case r of
        False -> do
            st <- get
            inicio <- runExpr e1
            final <- runExpr e2
            let start = fromInteger $ floor $ fromCNumber $ inicio
            let end = fromInteger $ floor $ fromCNumber $ final
            let symT = (SymTable M.empty (h st))
            modify $ modifyTable (pushTable (Run.insertSym s (CNumber(fromIntegral $ start)) Number False symT))
            modify $ modifyHeight (+1)
            runFor s end ins
            modify $ modifyHeight (\x -> x-1)
            modify (modifyTable popTable)
        True -> return ()

runAnidS (Bforby (Lexer.Identifier p s) e1 e2 e3 []) = do
    r <- verifyReturn
    case r of
        False -> do
            st <- get
            inicio <- runExpr e1
            final <- runExpr e2
            paso <- runExpr e3
            let start = fromCNumber inicio
            let end = fromCNumber final
            let step = fromCNumber paso
            let symT = (SymTable M.empty (h st))
            modify $ modifyTable (pushTable (Run.insertSym s (CNumber start) Number False symT))
            modify $ modifyHeight (+1)
            runForBy s end [] step
            modify $ modifyHeight (\x -> x-1)
            modify (modifyTable popTable)
        True -> return ()
            


runAnidS (Bforby (Lexer.Identifier p s) e1 e2 e3 ins) = do
    r <- verifyReturn
    case r of
        False -> do
            st <- get
            inicio <- runExpr e1
            final <- runExpr e2
            paso <- runExpr e3
            let start = fromCNumber $ inicio
            let end = fromCNumber $ final
            let step = fromCNumber paso
            let symT = (SymTable M.empty (h st))
            modify $ modifyTable (pushTable (Run.insertSym s (CNumber start) Number False symT))
            modify $ modifyHeight (+1)
            runForBy s end ins step
            modify $ modifyHeight (\x -> x-1)
            modify (modifyTable popTable)
        True -> return ()

runAnidS (Brepeat e [] p) = do
    r <- verifyReturn
    case r of
        False -> do
            v <- runExpr e
            let n = numberConversionHandler $ fromCNumber v
            case n < 0 of
                True -> throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en instruccion repeat, contador negativo")
                False -> do
                    runRep n []
        True -> return ()



runAnidS (Brepeat e ins p) = do
    r <- verifyReturn
    case r of
        False -> do
            v <- runExpr e
            let n = numberConversionHandler $ fromCNumber v
            case n < 0 of
                True -> throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                    ++ (Out.printPos p)
                                    ++ " en instruccion repeat, contador negativo")
                False -> do
                    runRep n ins
        True -> return ()



runAnidS (Out.Asig lt e) = do
    r <- verifyReturn
    case r of
        False -> do
            let p = takePos lt
            let s = takeStr lt
            v <- runExpr e
            st <- get
            let k = fromJust $ findSym s $ tablas st
            case mutable k of
                True -> do
                    let (x,n) = fromJust $ findSymTable s (tablas st) 0
                    let x1 = (SymTable (M.insert s (v,(tipo k),True) $ mapa x) $ height x)
                    modify $ modifyTable $ replaceAt n x1
                False -> throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                ++ (Out.printPos p)
                                ++ " en instruccion de asignacion, variable no mutable: "
                                ++ s)
        True -> return ()

runAnidS (InsFcall f) = do
    r <- verifyReturn
    case r of
        False -> do   
            runProc f
        True -> return ()


runAnidS (Out.Read lt) = do
    r <- verifyReturn
    case r of
        False -> do
            let p = takePos lt
            let s = takeStr lt
            st <- get
            let k = fromJust $ findSym s $ tablas st
            case mutable k of
                True -> do
                    val <- liftIO $ getLine
                    case tipo k of
                        Number -> do
                            let readValue = Tx.readMaybe val :: Maybe Double
                            case readValue of
                                Nothing -> do
                                    throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                                                        ++ (Out.printPos p)
                                                                        ++ " en instruccion de lectura, se esperaba un numero")
                                Just finalValue -> do
                                    let vc = CNumber finalValue
                                    let (x,n) = fromJust $ findSymTable s (tablas st) 0
                                    let x1 = (SymTable (M.insert s (vc,Number,True) $ mapa x) $ height x)
                                    modify $ modifyTable $ replaceAt n x1
                        Boolean -> do
                            case val of
                                "true" -> do
                                    let vc = CBoolean True
                                    let (x,n) = fromJust $ findSymTable s (tablas st) 0
                                    let x1 = (SymTable (M.insert s (vc,Boolean,True) $ mapa x) $ height x)
                                    modify $ modifyTable $ replaceAt n x1

                                "false" -> do
                                    let vc = CBoolean False
                                    let (x,n) = fromJust $ findSymTable s (tablas st) 0
                                    let x1 = (SymTable (M.insert s (vc,Boolean,True) $ mapa x) $ height x)
                                    modify $ modifyTable $ replaceAt n x1

                                _ -> throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                                ++ (Out.printPos p)
                                                ++ " en instruccion de lectura, se esperaba un booleano.")
        True -> return ()
runAnidS (Write args) = do
    r <- verifyReturn
    case r of
        False -> do
            runExprS args
            liftIO (putStr "" >> hFlush stdout)
        True -> return ()
runAnidS (WriteLn args) = do
    r <- verifyReturn
    case r of
        False -> do
            runExprS args
            liftIO (putStr "" >> hFlush stdout)
        True -> return ()

runAnidS (Return e p) = do
    r <- verifyReturn
    case r of
        False -> do
            val <- runExpr e
            modify $ modifyHandler $ replace (Just val)
        True -> return ()

runAnidS (EmptyB) = do
    return ()
        
