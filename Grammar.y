{
{-# LANGUAGE DeriveDataTypeable #-}
module Grammar where
import qualified Lexer
import Stdout
import Control.Exception
import Data.Typeable
}

%name parse
%tokentype { Lexer.Token }
%error { parseError }
%monad { IO }

%token

    '%'         { (Lexer.Modex _ _)         }
    '/'         { (Lexer.Divex _ _)         }
    '*'         { (Lexer.Mult _ _)          }
    '-'         { (Lexer.Minus _ _)         }
    ')'         { (Lexer.CloseP _ _)        }     
    '('         { (Lexer.OpenP _ _)         }     
    '+'         { (Lexer.Plus _ _)          }     
    '='         { (Lexer.Def _ _)           }     
    ';'         { (Lexer.SemiColon _ _)     }     
    ','         { (Lexer.Comma _ _)         }     
    '<'         { (Lexer.Less _ _)          }     
    '>'         { (Lexer.More _ _)          }     
    '=='        { (Lexer.Eq _ _)            }      
    '/='        { (Lexer.Neq _ _)           }      
    '>='        { (Lexer.Moreq _ _)         }      
    '<='        { (Lexer.Lesseq _ _)        }      
    '->'        { (Lexer.Arrow _ _)         }      
    not         { (Lexer.Not _ _)           }    
    and         { (Lexer.And _ _)           }    
    or          { (Lexer.Or _ _)            }    
    div         { (Lexer.Div _ _)           }    
    mod         { (Lexer.Mod _ _)           }    
    number      { (Lexer.Number _ _)        }    
    boolean     { (Lexer.Boolean _ _)       }    
    true        { (Lexer.True' _ _)         }    
    false       { (Lexer.False' _ _)        }    
    with        { (Lexer.With _ _)          }    
    do          { (Lexer.Do _ _)            }    
    end         { (Lexer.End _ _)           }    
    if          { (Lexer.If _ _)            }    
    then        { (Lexer.Then _ _)          }    
    else        { (Lexer.Else _ _)          }    
    while       { (Lexer.While _ _)         }    
    for         { (Lexer.For _ _)           }    
    repeat      { (Lexer.Repeat _ _)        }    
    begin       { (Lexer.Begin _ _)         }    
    return      { (Lexer.Return _ _)        }    
    func        { (Lexer.Func _ _)          }    
    times       { (Lexer.Times _ _)         }    
    program     { (Lexer.Program _ _)       }    
    integer     { (Lexer.Integer _  _)      }   
    floating    { (Lexer.Floating _  _)     }   
    str         { (Lexer.Str _ _)           }   
    identifier  { (Lexer.Identifier _ _)    }
    writeln     { (Lexer.WriteLn _ _)       }
    write       { (Lexer.Write _ _)         }
    read        { (Lexer.Read _ _)          }
    by          { (Lexer.By _ _)            }
    from        { (Lexer.From _ _)          }
    to          { (Lexer.To _ _)            }

%left '-' '+' or
%left '%' mod
%left '*' '/' div and
%nonassoc '>' '<' '<=' '>=' '==' '/='
%right not NEG

%%

Init    : ListaF program Bloque end';'   {Program (reverse $1) (reverse $3)}

ListaF  : ListaF FunDec     {$2 : $1}
        | {- lambda -}      {[]}

FunDec  : func identifier'('Param')' begin Bloque end';'                {Proc $2 (reverse $4) (reverse $7)}
        | func identifier'('Param')' '->' Tipo begin Bloque end';'      {Func $2 (reverse $4) $7 (reverse $9)}

Bloque  : {- lambda -}      {[]}
        | Bloque AnidS      {$2 : $1}

AnidS   : if Expr then Bloque else Bloque end';'                        {Bifelse $2 (reverse $4) (reverse $6) (takePos $1)}
        | if Expr then Bloque end';'                                    {Bif $2 (reverse $4) (takePos $1)}
        | with ListaD do Bloque end';'                                  {Bwith (reverse $2) (reverse $4)}
        | while Expr do Bloque end';'                                   {Bwhile $2 (reverse $4) (takePos $1)}
        | for identifier from Expr to Expr do Bloque end';'             {Bfor $2 $4 $6 (reverse $8)}
        | for identifier from Expr to Expr by Expr do Bloque end';'     {Bforby $2 $4 $6 $8 (reverse $10)}
        | repeat Expr times Bloque end';'                               {Brepeat $2 (reverse $4) (takePos $1)}
        | identifier '=' Expr';'                                        {Asig $1 $3}
        | Funcion';'                                                    {InsFcall $1}
        | read identifier ';'                                           {Read $2}
        | write ArgW ';'                                                {Write (reverse $2)}
        | writeln ArgW';'                                               {WriteLn (reverse $2)}
        | return Expr';'                                                {Return $2 (takePos $1)}
        | ';'                                                           {EmptyB}

        
Param   : {- lambda -}      {[]}
        | ParamD            {$1}

ParamD  : Tipo identifier               {[ParamL $1 $2]}
        | ParamD ',' Tipo identifier    {(ParamL $3 $4) : $1}


ListaD  : {- lambda -}                   {[]}
        | ListaD Decl                    {$2 : $1}

Decl    : Tipo identifier '=' Expr';'   {Inicializacion $1 $2 $4}
        | Tipo ListaI';'                {Declaracion $1 $2}
        | ';'                           {EmptyD}

Tipo    : number                        {NumberT}
        | boolean                       {BooleanT}

ListaI  : identifier                    {[$1]}
        | ListaI ',' identifier         {$3 : $1}

ArgW    : ExprS                         {[$1]}
        | ArgW ',' ExprS                {$3 : $1}


ExprS   : Expr                          {ExprW $1}
        | str                           {StringW $1}

Args    : Expr                          {[$1]}
        | Args ',' Expr                 {$3 : $1}

Funcion : identifier'('')'                                           {FuncionSA $1}
        | identifier'(' Args ')'                                     {FuncionCA $1 (reverse $3)}
        
Expr    : Expr or Expr                  {Or $1 $3 (takePos $2)}
        | Expr and Expr                 {And $1 $3 (takePos $2)}
        | Expr '==' Expr                {Eq $1 $3 (takePos $2)}
        | Expr '/=' Expr                {Neq $1 $3 (takePos $2)}
        | Expr '<' Expr                 {Less $1 $3 (takePos $2)}
        | Expr '<=' Expr                {Lesseq $1 $3 (takePos $2)}
        | Expr '>' Expr                 {More $1 $3 (takePos $2)}
        | Expr '>=' Expr                {Moreq $1 $3 (takePos $2)}
        | Expr '+' Expr                 {Plus $1 $3 (takePos $2)}
        | Expr '-' Expr                 {Minus $1 $3 (takePos $2)}
        | Expr '*' Expr                 {Mult $1 $3 (takePos $2)}
        | Expr '/' Expr                 {Divex $1 $3 (takePos $2)}
        | Expr '%' Expr                 {Modex $1 $3 (takePos $2)}
        | Expr div Expr                 {Div $1 $3 (takePos $2)}
        | Expr mod Expr                 {Mod $1 $3 (takePos $2)}
        | not Expr                      {Not $2 (takePos $1)}
        | '-'Expr %prec NEG             {Uminus $2 (takePos $1)}
        | identifier                    {Identifier $1}
        | integer                       {Integer $1}
        | floating                      {Floating $1}
        | true                          {ExpTrue $1}
        | false                         {ExpFalse $1}
        | Funcion                       {ExpFcall $1}
        | '(' Expr ')'                  {Bracket $2}
{

data SyntacticError = SyntacticError String
    deriving (Typeable)

instance Exception SyntacticError
instance Show SyntacticError where
    show (SyntacticError s) = "Error sintáctico: " ++ s

parseError :: [Lexer.Token] -> IO a
parseError [] = throw $ SyntacticError "Archivo Vacio"
parseError ts = throw $ SyntacticError $ (makePrintable (head ts)) ++ " Token inesperado"


}