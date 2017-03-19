module Stdout where
-- Lets print everything
import qualified Lexer

-- Token List printer
getPos :: Lexer.AlexPosn -> (Int,Int)
getPos (Lexer.AlexPn _ l c) = (l,c)


printPos :: Lexer.AlexPosn -> String
printPos (Lexer.AlexPn _ l c) = "linea " ++ show(l) ++ ", columna " ++ show(c)


printPlease :: [Lexer.Token] -> [String]
printPlease = foldr (\x acc -> (makePrintable x) : acc) []


-- Token printers
makePrintable :: Lexer.Token -> String

makePrintable (Lexer.Integer p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": literal numerico '" ++ (show s) ++ "'"

makePrintable (Lexer.Floating p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": literal numerico '" ++ (show s) ++ "'"

makePrintable (Lexer.Str p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": cadena de caracteres " ++ (id s)

makePrintable (Lexer.Identifier p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": identificador '" ++ (id s) ++ "'"

makePrintable (Lexer.Modex p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Divex p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Mult p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Minus p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.CloseP p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.OpenP p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Plus p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Def p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.SemiColon p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Comma p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Less p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.More p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Not p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.And p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Or p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Eq p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Neq p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Moreq p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Lesseq p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Div p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Mod p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Arrow p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": signo '" ++ (id s) ++ "'"

makePrintable (Lexer.Number p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": tipo de dato '" ++ (id s) ++ "'"

makePrintable (Lexer.Boolean p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": tipo de dato '" ++ (id s) ++ "'"

makePrintable (Lexer.True' p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": literal booleano '" ++ (id s) ++ "'"

makePrintable (Lexer.False' p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": literal booleano '" ++ (id s) ++ "'"

makePrintable (Lexer.With p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.Do p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
    
makePrintable (Lexer.End p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
   
makePrintable (Lexer.If p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
    
makePrintable (Lexer.Else p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
  
makePrintable (Lexer.Then p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
  
makePrintable (Lexer.While p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
 
makePrintable (Lexer.For p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
   
makePrintable (Lexer.Repeat p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.Begin p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
 
makePrintable (Lexer.Return p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.Func p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
  
makePrintable (Lexer.Times p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"
 
makePrintable (Lexer.Program p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.WriteLn p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.Write p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.By p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.From p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.To p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.Read p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": palabra reservada '" ++ (id s) ++ "'"

makePrintable (Lexer.LexError p s) = "linea " ++ show(fst(getPos p)) ++ ", columna " ++ show(snd(getPos p)) ++ ": Caracter inesperado '" ++ (id s) ++ "'"


takePos :: Lexer.Token -> Lexer.AlexPosn
takePos (Lexer.Integer p s) = p

takePos (Lexer.Floating p s) = p

takePos (Lexer.Str p s) = p

takePos (Lexer.Identifier p s) = p

takePos (Lexer.Modex p s) = p

takePos (Lexer.Divex p s) = p

takePos (Lexer.Mult p s) = p

takePos (Lexer.Minus p s) = p

takePos (Lexer.CloseP p s) = p

takePos (Lexer.OpenP p s) = p

takePos (Lexer.Plus p s) = p

takePos (Lexer.Def p s) = p

takePos (Lexer.SemiColon p s) = p

takePos (Lexer.Comma p s) = p

takePos (Lexer.Less p s) = p

takePos (Lexer.More p s) = p

takePos (Lexer.Not p s) = p

takePos (Lexer.And p s) = p

takePos (Lexer.Or p s) = p

takePos (Lexer.Eq p s) = p

takePos (Lexer.Neq p s) = p

takePos (Lexer.Moreq p s) = p

takePos (Lexer.Lesseq p s) = p

takePos (Lexer.Div p s) = p

takePos (Lexer.Mod p s) = p

takePos (Lexer.Arrow p s) = p

takePos (Lexer.Number p s) = p

takePos (Lexer.Boolean p s) = p

takePos (Lexer.True' p s) = p

takePos (Lexer.False' p s) = p

takePos (Lexer.With p s) = p

takePos (Lexer.Do p s) = p

takePos (Lexer.End p s) = p

takePos (Lexer.If p s) = p

takePos (Lexer.Else p s) = p

takePos (Lexer.Then p s) = p

takePos (Lexer.While p s) = p

takePos (Lexer.For p s) = p

takePos (Lexer.Repeat p s) = p

takePos (Lexer.Begin p s) = p

takePos (Lexer.Return p s) = p

takePos (Lexer.Func p s) = p

takePos (Lexer.Times p s) = p

takePos (Lexer.Program p s) = p

takePos (Lexer.WriteLn p s) = p

takePos (Lexer.Write p s) = p

takePos (Lexer.By p s) = p

takePos (Lexer.From p s) = p

takePos (Lexer.To p s) = p

takePos (Lexer.Read p s) = p

takePos (Lexer.LexError p s) = p



takeStr :: Lexer.Token -> String
takeStr (Lexer.Integer p s) = s

takeStr (Lexer.Floating p s) = s

takeStr (Lexer.Str p s) = s

takeStr (Lexer.Identifier p s) = s

takeStr (Lexer.Modex p s) = s

takeStr (Lexer.Divex p s) = s

takeStr (Lexer.Mult p s) = s

takeStr (Lexer.Minus p s) = s

takeStr (Lexer.CloseP p s) = s

takeStr (Lexer.OpenP p s) = s

takeStr (Lexer.Plus p s) = s

takeStr (Lexer.Def p s) = s

takeStr (Lexer.SemiColon p s) = s

takeStr (Lexer.Comma p s) = s

takeStr (Lexer.Less p s) = s

takeStr (Lexer.More p s) = s

takeStr (Lexer.Not p s) = s

takeStr (Lexer.And p s) = s

takeStr (Lexer.Or p s) = s

takeStr (Lexer.Eq p s) = s

takeStr (Lexer.Neq p s) = s

takeStr (Lexer.Moreq p s) = s

takeStr (Lexer.Lesseq p s) = s

takeStr (Lexer.Div p s) = s

takeStr (Lexer.Mod p s) = s

takeStr (Lexer.Arrow p s) = s

takeStr (Lexer.Number p s) = s

takeStr (Lexer.Boolean p s) = s

takeStr (Lexer.True' p s) = s

takeStr (Lexer.False' p s) = s

takeStr (Lexer.With p s) = s

takeStr (Lexer.Do p s) = s

takeStr (Lexer.End p s) = s

takeStr (Lexer.If p s) = s

takeStr (Lexer.Else p s) = s

takeStr (Lexer.Then p s) = s

takeStr (Lexer.While p s) = s

takeStr (Lexer.For p s) = s

takeStr (Lexer.Repeat p s) = s

takeStr (Lexer.Begin p s) = s

takeStr (Lexer.Return p s) = s

takeStr (Lexer.Func p s) = s

takeStr (Lexer.Times p s) = s

takeStr (Lexer.Program p s) = s

takeStr (Lexer.WriteLn p s) = s

takeStr (Lexer.Write p s) = s

takeStr (Lexer.By p s) = s

takeStr (Lexer.From p s) = s

takeStr (Lexer.To p s) = s

takeStr (Lexer.Read p s) = s

takeStr (Lexer.LexError p s) = s


-- Show token Value
instance Show Lexer.Token where
    show (Lexer.Integer _ s) = "Literal Numerico: '" ++ (show s) ++ "'"

    show (Lexer.Floating _ s) = "Literal Numerico: '" ++ (show s) ++ "'"

    show (Lexer.Str _ s) = "Cadena de Caracteres: " ++ (id s)

    show (Lexer.Identifier _ s) = "Identificador: '" ++ (id s) ++ "'"

    show (Lexer.Modex _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Divex _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Mult _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Minus _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.CloseP _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.OpenP _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Plus _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Def _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.SemiColon _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Comma _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Less _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.More _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Not _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.And _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Or _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Eq _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Neq _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Moreq _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Lesseq _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Div _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Mod _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Arrow _ s) = "Signo: '" ++ (id s) ++ "'"

    show (Lexer.Number _ s) = "Tipo de Dato: '" ++ (id s) ++ "'"

    show (Lexer.Boolean _ s) = "Tipo de Dato: '" ++ (id s) ++ "'"

    show (Lexer.True' _ s) = "Literal Booleano: '" ++ (id s) ++ "'"

    show (Lexer.False' _ s) = "Literal Booleano: '" ++ (id s) ++ "'"

    show (Lexer.With _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"

    show (Lexer.Do _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
        
    show (Lexer.End _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
       
    show (Lexer.If _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
        
    show (Lexer.Else _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
      
    show (Lexer.Then _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
      
    show (Lexer.While _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
     
    show (Lexer.For _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
       
    show (Lexer.Repeat _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"

    show (Lexer.Begin _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
     
    show (Lexer.Return _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"

    show (Lexer.Func _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
      
    show (Lexer.Times _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
     
    show (Lexer.Program _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"

    show (Lexer.WriteLn _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"

    show (Lexer.Write _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"

    show (Lexer.By _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"

    show (Lexer.From _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"

    show (Lexer.To _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"

    show (Lexer.Read _ s) = "Palabra Reservada: '" ++ (id s) ++ "'"
    
    show (Lexer.LexError _ s) = "Caracter Inesperado: '" ++ (id s) ++ "'" 


data Init     = Program [FunDec] [AnidS] deriving (Show)

data FunDec   = Proc Lexer.Token [ParamL] [AnidS]                |
                Func Lexer.Token [ParamL] Tipo [AnidS]
                deriving (Show)

data ParamL   = ParamL Tipo Lexer.Token deriving (Show)

data AnidS    = Bifelse Expr [AnidS] [AnidS] Lexer.AlexPosn                         |
                Bif Expr [AnidS] Lexer.AlexPosn                                     |
                Bwith [Decl] [AnidS]                                                |
                Bwhile Expr [AnidS] Lexer.AlexPosn                                  |
                Bfor Lexer.Token Expr Expr [AnidS]                                  |
                Bforby Lexer.Token Expr Expr Expr [AnidS]                           |
                Brepeat Expr [AnidS] Lexer.AlexPosn                                 |
                Asig {leftSide :: Lexer.Token, rightSide :: Expr}                   |
                InsFcall Funcion                                                    |
                Read Lexer.Token                                                    |
                Write [ExprS]                                                       |
                WriteLn [ExprS]                                                     |
                Return Expr Lexer.AlexPosn                                          |
                EmptyB                                       
                deriving (Show)

data Decl     = Inicializacion Tipo Lexer.Token Expr        |
                Declaracion Tipo [Lexer.Token]              |
                EmptyD
                deriving (Show)

data Tipo     = NumberT |
                BooleanT
                deriving (Eq,Show)

data ExprS    = ExprW Expr           |
                StringW Lexer.Token  
                deriving (Show)

data Funcion  = FuncionSA Lexer.Token         |
                FuncionCA Lexer.Token [Expr]
                deriving (Show)

data Expr     = Or Expr Expr Lexer.AlexPosn                   |
                And Expr Expr Lexer.AlexPosn                  |
                Eq Expr Expr  Lexer.AlexPosn                  |
                Neq Expr Expr  Lexer.AlexPosn                 |
                Less Expr Expr  Lexer.AlexPosn                |
                Lesseq Expr Expr  Lexer.AlexPosn              |
                More Expr Expr Lexer.AlexPosn                 |
                Moreq Expr Expr Lexer.AlexPosn                |
                Plus Expr Expr Lexer.AlexPosn                 |
                Minus Expr Expr Lexer.AlexPosn                |
                Mult Expr Expr Lexer.AlexPosn                 |
                Divex Expr Expr Lexer.AlexPosn                |
                Modex Expr Expr Lexer.AlexPosn                |
                Div Expr Expr Lexer.AlexPosn                  |
                Mod Expr Expr  Lexer.AlexPosn                 |
                Not Expr Lexer.AlexPosn                       |
                Uminus Expr Lexer.AlexPosn                    |
                Identifier Lexer.Token                        |
                Integer Lexer.Token                           |
                Floating Lexer.Token                          |
                ExpTrue Lexer.Token                           |
                ExpFalse Lexer.Token                          |
                ExpFcall Funcion                              |
                Bracket Expr
                deriving (Show)



showInit :: String -> Int -> Init -> String

showInit sep h (Program [] li) = 
    (showLine sep h "Programa principal:\n") ++ (concatMap (showAnidS sep (h+1)) li)

showInit sep h (Program lf li) =
    (showLine sep h "Lista de Declaraciones de funciones:\n") ++ (concatMap (showFunDec sep (h+1)) lf)
        ++ (showLine sep h "Programa principal:\n") ++ (concatMap (showAnidS sep (h+1)) li)


showFunDec :: String -> Int -> FunDec -> String

showFunDec sep h (Proc idt [] []) =
    (showLine sep h "Declaracion de funcion:\n") ++ (showLine sep (h+1) "Nombre:\n")
    ++ (showLine sep (h+2) (show idt)) ++ "\n" ++ (showLine sep (h+1) "Parametros: Vacio\n")
    ++ (showLine sep (h+1) "Instrucciones: Vacio\n")

showFunDec sep h (Proc idt [] ins) =
    (showLine sep h "Declaracion de funcion:\n") ++ (showLine sep (h+1) "Nombre:\n")
    ++ (showLine sep (h+2) (show idt)) ++ "\n" ++ (showLine sep (h+1) "Parametros: Vacio\n")
    ++ (showLine sep (h+1) "Instrucciones:\n") ++ (concatMap (showAnidS sep (h+2)) ins)

showFunDec sep h (Proc idt at []) =
    (showLine sep h "Declaracion de funcion:\n") ++ (showLine sep (h+1) "Nombre:\n")
    ++ (showLine sep (h+2) (show idt)) ++ "\n" ++ (showLine sep (h+1) "Parametros:\n")
    ++ (concatMap (showParamL sep (h+2)) at) ++ (showLine sep (h+1) "Instrucciones: Vacio\n")

showFunDec sep h (Proc idt at ins) =
    (showLine sep h "Declaracion de funcion:\n") ++ (showLine sep (h+1) "Nombre:\n")
    ++ (showLine sep (h+2) (show idt)) ++ "\n" ++ (showLine sep (h+1) "Parametros:\n")
    ++ (concatMap (showParamL sep (h+2)) at) ++ (showLine sep (h+1) "Instrucciones:\n")
    ++ (concatMap (showAnidS sep (h+2)) ins)

showFunDec sep h (Func idt [] t []) =
    (showLine sep h "Declaracion de funcion:\n") ++ (showLine sep (h+1) "Nombre:\n")
    ++ (showLine sep (h+2) (show idt)) ++ "\n" ++ (showLine sep (h+1) "Parametros: Vacio\n")
    ++ (showTipo sep (h+1) t) ++ (showLine sep (h+1) "Instrucciones: Vacio\n")

showFunDec sep h (Func idt [] t ins) =
    (showLine sep h "Declaracion de funcion:\n") ++ (showLine sep (h+1) "Nombre:\n")
    ++ (showLine sep (h+2) (show idt)) ++ "\n" ++ (showLine sep (h+1) "Parametros: Vacio\n")
    ++ (showTipo sep (h+1) t) ++ (showLine sep (h+1) "Instrucciones:\n")
    ++ (concatMap (showAnidS sep (h+2)) ins)

showFunDec sep h (Func idt at t []) =
    (showLine sep h "Declaracion de funcion:\n") ++ (showLine sep (h+1) "Nombre:\n")
    ++ (showLine sep (h+2) (show idt)) ++ "\n" ++ (showLine sep (h+1) "Parametros:\n")
    ++ (concatMap (showParamL sep (h+2)) at) ++ (showTipo sep (h+1) t)
    ++ (showLine sep (h+1) "Instrucciones: Vacio\n")

showFunDec sep h (Func idt at t ins) =
    (showLine sep h "Declaracion de funcion:\n") ++ (showLine sep (h+1) "Nombre:\n")
    ++ (showLine sep (h+2) (show idt)) ++ "\n" ++ (showLine sep (h+1) "Parametros:\n")
    ++ (concatMap (showParamL sep (h+2)) at) ++ (showTipo sep (h+1) t)
    ++ (showLine sep (h+1) "Instrucciones:\n") ++ (concatMap (showAnidS sep (h+2)) ins)




showParamL :: String -> Int -> ParamL -> String
showParamL sep h (ParamL t id) = 
  (showLine sep h "Parametro de Declaracion de Funcion:\n") ++ (showTipo sep (h+1) t) 
  ++ (showLine sep (h+1) ((show id) ++ "\n")) 


showAnidS :: String -> Int -> AnidS -> String
showAnidS sep h (Bifelse e [] [] _) = 
  (showLine sep h "Instruccion de Control If Else:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones de Caso Verdadero: Vacio\n") 
    ++ (showLine sep (h+1) "Instrucciones de Caso Falso: Vacio\n")

showAnidS sep h (Bifelse e ins1 [] _) = 
  (showLine sep h "Instruccion de Control IF Else:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones de Caso Verdadero:\n") 
    ++ (concatMap (showAnidS sep (h+2)) ins1) ++ (showLine sep (h+1) "Instrucciones de Caso Falso: Vacio\n")

showAnidS sep h (Bifelse e [] ins2 _) = 
  (showLine sep h "Instruccion de Control IF Else:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones de Caso Verdadero: Vacio\n")
    ++ (showLine sep (h+1) "Instrucciones de Caso Falso:\n") ++ (concatMap (showAnidS sep (h+2)) ins2)

showAnidS sep h (Bifelse e ins1 ins2 _) = 
  (showLine sep h "Instruccion de Control IF Else:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones de Caso Verdadero:\n") 
    ++ (concatMap (showAnidS sep (h+2)) ins1) ++ (showLine sep (h+1) "Instrucciones de Caso Falso:\n") 
    ++ (concatMap (showAnidS sep (h+2)) ins2)

showAnidS sep h (Bif e [] _) = 
  (showLine sep h "Instruccion de Control If:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones: Vacio\n") 

showAnidS sep h (Bif e ins _) = 
  (showLine sep h "Instruccion de Control IF:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones:\n") 
    ++ (concatMap (showAnidS sep (h+2)) ins)

showAnidS sep h (Bwith [] []) = 
  (showLine sep h "Instruccion de Control With:\n") ++ (showLine sep (h+1) "Declaraciones: Vacio\n")
    ++ (showLine sep (h+1) "Instrucciones: Vacio\n")

showAnidS sep h (Bwith [] ins) = 
  (showLine sep h "Instruccion de Control With:\n") ++ (showLine sep (h+1) "Declaraciones: Vacio\n")
    ++ (showLine sep (h+1) "Instrucciones:\n") ++ (concatMap (showAnidS sep (h+2)) ins) 

showAnidS sep h (Bwith dls []) = 
  (showLine sep h "Instruccion de Control With:\n") ++ (showLine sep (h+1) "Declaraciones:\n") 
    ++ (concatMap (showDecl sep (h+2)) dls) ++ (showLine sep (h+1) "Instrucciones: Vacio\n")

showAnidS sep h (Bwith dls ins) = 
  (showLine sep h "Instruccion de Control With:\n") ++ (showLine sep (h+1) "Declaraciones:\n") 
    ++ (concatMap (showDecl sep (h+2)) dls) ++ (showLine sep (h+1) "Instrucciones:\n") 
    ++ (concatMap (showAnidS sep (h+2)) ins) 

showAnidS sep h (Bwhile e [] _) = 
  (showLine sep h "Instruccion de Control For:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones: Vacio\n") 

showAnidS sep h (Bwhile e ins _) = 
  (showLine sep h "Instruccion de Control For:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones:\n") 
    ++ (concatMap (showAnidS sep (h+2)) ins)

showAnidS sep h (Bfor id e1 e2 []) = 
  (showLine sep h "Instruccion de Control For:\n") ++ (showLine sep (h+1) "Iterador:\n") 
    ++ (showLine sep (h+2) (show id)++"\n") ++ (showLine sep (h+1) "Inicio:\n") 
    ++ (showExpr sep (h+2) e1) ++ (showLine sep (h+1) "Fin:\n")
    ++ (showExpr sep (h+2) e2) ++ (showLine sep (h+1) "Instrucciones: Vacio\n")

showAnidS sep h (Bfor id e1 e2 ins) = 
  (showLine sep h "Instruccion de Control For:\n") ++ (showLine sep (h+1) "Iterador:\n") 
    ++ (showLine sep (h+2) (show id)++"\n") ++ (showLine sep (h+1) "Inicio:\n") 
    ++ (showExpr sep (h+2) e1) ++ (showLine sep (h+1) "Fin:\n")
    ++ (showExpr sep (h+2) e2) ++ (showLine sep (h+1) "Instrucciones:\n") 
    ++ (concatMap (showAnidS sep (h+2)) ins)

showAnidS sep h (Bforby id e1 e2 e3 []) = 
  (showLine sep h "Instruccion de Control For by:\n") ++ (showLine sep (h+1) "Iterador:\n") 
    ++ (showLine sep (h+2) (show id)++"\n") ++ (showLine sep (h+1) "Inicio:\n") 
    ++ (showExpr sep (h+2) e1) ++ (showLine sep (h+1) "Fin:\n")
    ++ (showExpr sep (h+2) e2) ++ (showLine sep (h+1) "Incremento:\n")
    ++ (showExpr sep (h+2) e3) ++ (showLine sep (h+1) "Instrucciones: Vacio\n")

showAnidS sep h (Bforby id e1 e2 e3 ins) = 
  (showLine sep h "Instruccion de Control For by:\n") ++ (showLine sep (h+1) "Iterador:\n") 
    ++ (showLine sep (h+2) (show id)++"\n") ++ (showLine sep (h+1) "Inicio:\n") 
    ++ (showExpr sep (h+2) e1) ++ (showLine sep (h+1) "Fin:\n")
    ++ (showExpr sep (h+2) e2) ++ (showLine sep (h+1) "Incremento:\n")
    ++ (showExpr sep (h+2) e3) ++ (showLine sep (h+1) "Instrucciones:\n") 
    ++ (concatMap (showAnidS sep (h+2)) ins)

showAnidS sep h (Brepeat e [] _) = 
  (showLine sep h "Instruccion de Control Repeat:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones: Vacio\n")

showAnidS sep h (Brepeat e ins _) = 
  (showLine sep h "Instruccion de Control Repeat:\n") ++ (showLine sep (h+1) "Condicion:\n") 
    ++ (showExpr sep (h+2) e) ++ (showLine sep (h+1) "Instrucciones:\n") 
    ++ (concatMap (showAnidS sep (h+2)) ins)

showAnidS sep h (Asig id e) = 
  (showLine sep h "Asignacion:\n") ++ (showLine sep (h+1) "Variable:\n") 
    ++ (showLine sep (h+2) (show id)++"\n") ++ (showLine sep (h+1) "Expresion Asignada:\n") 
    ++ (showExpr sep (h+2) e)

showAnidS sep h (InsFcall f) = showFuncion sep h f

showAnidS sep h (Read id) = 
  (showLine sep h "Instruccion: Read\n") ++ (showLine sep (h+1) "Argumento:\n")
    ++ (showLine sep (h+2) (show id)++"\n")

showAnidS sep h (Write xprs) = 
  (showLine sep h "Instruccion: Write\n") ++ (showLine sep (h+1) "Argumentos:\n")
    ++ (concatMap (showExprS sep (h+2)) xprs)

showAnidS sep h (WriteLn xprs) = 
  (showLine sep h "Instruccion: WriteLn\n") ++ (showLine sep (h+1) "Argumentos:\n")
    ++ (concatMap (showExprS sep (h+2)) xprs)

showAnidS sep h (Return e _) = (showLine sep h "Instruccion: Retorno\n") ++ (showExpr sep (h+2) e)

showAnidS sep h EmptyB = showLine sep h "Instruccion: Vacia\n"



showDecl :: String -> Int -> Decl -> String 
showDecl sep h (Inicializacion t id e) = 
  (showLine sep h "Declaracion:\n") ++ (showTipo sep (h+1) t)
    ++ (showLine sep (h+1) "Variable:\n") ++ (showLine sep (h+2) (show id)++"\n")
    ++ (showLine sep (h+1) "Inicializacion:\n") ++ (showExpr sep (h+2) e)

showDecl sep h (Declaracion t []) = (showLine sep h "Esto es un error porque no puedes hacer declaraciones sin id's\n")

showDecl sep h (Declaracion t ids) = 
  (showLine sep h "Declaracion:\n") ++ (showTipo sep (h+1) t)
    ++ (showLine sep (h+1) "Variables:\n") ++ (concatMap (\x -> showLine sep (h+2) ((show x) ++ "\n")) ids)

showDecl sep h EmptyD = showLine sep h "Declaracion: Vacia\n"


showTipo :: String -> Int -> Tipo -> String 
showTipo sep h NumberT = showLine sep h "Tipo de Datos: Number\n"

showTipo sep h BooleanT = showLine sep h "Tipo de Datos: Boolean\n"


showExprS :: String -> Int -> ExprS -> String 
showExprS sep h (ExprW e) = showExpr sep h e

showExprS sep h (StringW lt) = showLine sep h ((show lt) ++ "\n")


showFuncion :: String -> Int -> Funcion -> String 
showFuncion sep h (FuncionSA lt) = 
  (showLine sep h "Llamado a Funcion: \n") ++ (showLine sep (h+1) "Nombre:\n") ++ (showLine sep (h+2) (show lt) ++ "\n")

showFuncion sep h (FuncionCA lt xprs) = 
  (showLine sep h "Llamado a Funcion: \n") ++ (showLine sep (h+1) "Nombre:\n") ++ (showLine sep (h+2) (show lt) ++ "\n")
    ++ (showLine sep (h+1) "Argumentos:\n") ++ (concatMap (showExpr sep (h+2)) xprs)


showExpr :: String -> Int -> Expr -> String
showExpr sep h (Or e1 e2 _) = 
  (showLine sep h "Operador Or:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (And e1 e2 _) = 
  (showLine sep h "Operador And:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Eq e1 e2 _) = 
  (showLine sep h "Operador Comparacion Equivalente:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Neq e1 e2 _) = 
  (showLine sep h "Operador Comparacion Inequivalente:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Less e1 e2 _) = 
  (showLine sep h "Operador Comparacion Menor:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Lesseq e1 e2 _) = 
  (showLine sep h "Operador Comparacion Menor o Igual:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (More e1 e2 _) = 
  (showLine sep h "Operador Comparacion Mayor:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Moreq e1 e2 _) = 
  (showLine sep h "Operador Comparacion Mayor o Igual:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Plus e1 e2 _) = 
  (showLine sep h "Operador Suma:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Minus e1 e2 _) = 
  (showLine sep h "Operador Resta:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Mult e1 e2 _) = 
  (showLine sep h "Operador Multiplicacion:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Divex e1 e2 _) = 
  (showLine sep h "Operador Division Exacta '/':\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Modex e1 e2 _) = 
  (showLine sep h "Operador Resto Exacto '%':\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Div e1 e2 _) = 
  (showLine sep h "Operador Div:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Mod e1 e2 _) = 
  (showLine sep h "Operador Mod:\n") 
    ++ (showLine sep (h+1) "Operando Izquierdo:\n") ++ (showExpr sep (h+2) e1)
    ++ (showLine sep (h+1) "Operando Derecho:\n") ++ (showExpr sep (h+2) e2)

showExpr sep h (Not e _) = (showLine sep h "Operador Unario Not:\n") ++ (showExpr sep (h+1) e)

showExpr sep h (Uminus e _) = (showLine sep h "Operador Unario Menos:\n") ++ (showExpr sep (h+1) e)

showExpr sep h (Identifier i) = showLine sep h (show i ++ "\n")

showExpr sep h (Integer n) = showLine sep h (show n ++ "\n")

showExpr sep h (Floating n) = showLine sep h (show n ++ "\n")

showExpr sep h (ExpTrue b) = showLine sep h (show b ++ "\n")

showExpr sep h (ExpFalse b) = showLine sep h (show b ++ "\n")

showExpr sep h (ExpFcall f) = showFuncion sep h f

showExpr sep h (Bracket e) = (showLine sep h "Expresion Agrupada: \n") ++ (showExpr sep (h+1) e)


showLine :: String -> Int -> String -> String
showLine sep h st = (foldr (++) "" (replicate h sep)) ++ st


showFuncionPos :: Funcion -> String
showFuncionPos (FuncionSA (Lexer.Identifier p s)) = (printPos p) ++ ". La Funcion " ++ s
showFuncionPos (FuncionCA (Lexer.Identifier p s) _) = printPos p ++ ". La Funcion " ++ s


showExprPos :: Expr -> String
showExprPos (Identifier (Lexer.Identifier p s)) = printPos p ++ ". La Variable " ++ s

