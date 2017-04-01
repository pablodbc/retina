module Programa where
import qualified Lexer as Lexer
import Stdout as Out
import qualified Grammar 
import Data.Map.Strict as M 
import Control.Monad.State.Strict
import Control.Exception
import InstruccionesRun
import Run
import Prelude as P


runFunDec :: Out.FunDec -> RunMonad ()
runFunDec (Out.Proc (Lexer.Identifier p s) params ins) = do
    modify $ insertFunProto s (FunProto (getTypeList params) (getStrList params) ins)


runFunDec (Out.Func idt@(Lexer.Identifier p s) params t ins) = do
    modify $ insertFunProto s (FunProto (getTypeList params) (getStrList params) ins)





anaInit :: Init -> RunMonad ()

anaInit (Program [] []) = do
    return ()

anaInit (Program [] ins) = do
    modify $ modifyHeight (+1)
    mapM_ runAnidS ins
    modify $ modifyHeight (\x -> x-1)
    return $! ()

anaInit (Program fs []) = do
    return $! ()

anaInit (Program fs ins) = do
    mapM_ runFunDec fs
    modify $ modifyHandler $ replace Nothing
    modify $ modifyHeight (+1)
    mapM_ runAnidS ins
    modify $ modifyHeight (\x -> x-1)
    return $! ()
