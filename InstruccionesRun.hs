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
import Text.Read

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
    r <- verifyReturn
        case r of
            False -> do
                st <- get
                inicio <- runExpr e1
                final <- runExpr e2
                let start = fromInteger $ floor $ inicio
                let end = fromInteger $ floor $ final
                let symT = (SymTable M.empty (h st))
                modify $ modifyTable (pushTable (Run.insertSym symT s start False))
                modify $ modifyHeight (+1)
                runFor s end [] 1
                modify $ modifyHeight (\x -> x-1)
                modify (modifyTable popTable)
            True -> return ()

-- Utilidad para correr un ciclo despues de insertar la tabla donde esta el simbolo no mutable

runFor :: String -> Integer -> Integer -> [AnidS] -> RunMonad ()
runFor s end [] step = do
    st <- get
    let var = fromJust $ findSym s $ tablas st
    let k = valor var
    case operateDoubleValCalc (<) k (CNumber end) of
        CBoolean True -> do
            let newK = modifyDoubleValCalc (+step) k
            let (x,n) = fromJust $ findSymTable s $ tablas st
            let x1 = (SymTable (insert s (newK,(tipo var),False) $ mapa x) $ height x)
            modify $ modifyTable $ replaceAt n x1
            runFor s end [] step
        CBoolean False -> return ()

runFor s end ins step = do
    st <- get
    let var = fromJust $ findSym s $ tablas st
    let k = valor var
    case operateDoubleValCalc (<) k (CNumber end) of
        CBoolean True -> do
            mapM_ runAnidS ins
            let newK = modifyDoubleValCalc (+step) k
            let (x,n) = fromJust $ findSymTable s $ tablas st
            let x1 = (SymTable (insert s (newK,(tipo var)False) $ mapa x) $ height x)
            modify $ modifyTable $ replaceAt n x1
            runFor s end ins step
        CBoolean False -> return ()

runAnidS (Bfor (Lexer.Identifier p s) e1 e2 ins) = do
    r <- verifyReturn
        case r of
            False -> do
                st <- get
                inicio <- runExpr e1
                final <- runExpr e2
                let start = fromInteger $ floor $ fromCNumber $ inicio
                let end = fromInteger $ floor $ fromCNumber $ final
                let symT = (SymTable M.empty (h st))
                modify $ modifyTable (pushTable (Run.insertSym symT s start False))
                modify $ modifyHeight (+1)
                runFor s end ins 1
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
                modify $ modifyTable (pushTable (Run.insertSym symT s start False))
                modify $ modifyHeight (+1)
                runFor s end [] step
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
                modify $ modifyTable (pushTable (Run.insertSym symT s start False))
                modify $ modifyHeight (+1)
                runFor s end ins step
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

runRep :: Int -> [AnidS] -> RunMonad ()
runRep 0 _ = return ()
runRep n [] = runRep (n-1) []
runRep n ins = do
    mapM_ runAnidS ins
    runRep (n-1) ins


runAnidS (Asig lt e) = do
    r <- verifyReturn
        case r of
            False -> do
                let p = takePos lt
                let s = takeStr lt
                v <- anaExpr e
                st <- get
                let k = fromJust $ findSym s $ tablas st
                case mutable k of
                    True -> do
                        let (x,n) = fromJust $ findSymTable s $ tablas st
                        let x1 = (SymTable (insert s (v,(tipo k),True) $ mapa x) $ height x)
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

runProc :: Funcion -> RunMonad ()
runProc (FuncionSA lt) = do
    let p = takePos lt
    let s = takeStr lt
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

runAnidS (Read lt) = do
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
                                let readValue = readMaybe val :: Maybe Double
                                case readValue of
                                    Nothing -> do
                                        throw $ Run.RuntimeError ("Cerca de la siguiente posicion" 
                                                                            ++ (Out.printPos p)
                                                                            ++ " en instruccion de lectura, se esperaba un numero)
                                    Just finalValue -> do
                                        let vc = CNumber finalValue
                                        let (x,n) = fromJust $ findSymTable s $ tablas st
                                        let x1 = (SymTable (insert s (vc,Number,True) $ mapa x) $ height x)
                                        modify $ modifyTable $ replaceAt n x1
                            Boolean -> do
                                case val of
                                    "true" -> do
                                        let vc = CBoolean True
                                        let (x,n) = fromJust $ findSymTable s $ tablas st
                                        let x1 = (SymTable (insert s (vc,Boolean,True) $ mapa x) $ height x)
                                        modify $ modifyTable $ replaceAt n x1

                                    "false" -> do
                                        let vc = CBoolean False
                                        let (x,n) = fromJust $ findSymTable s $ tablas st
                                        let x1 = (SymTable (insert s (vc,Boolean,True) $ mapa x) $ height x)
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
            modify $ modifyHandler $ replace Just val
        True -> return ()

runAnidS (EmptyB) = do
    return ()
        
