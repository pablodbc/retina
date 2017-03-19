{
module Lexer where
}
%wrapper "monadUserState"

$digit = 0-9
$letter = [a-zA-Z]
$special =  [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/\&\:\=\@\'\<\>\%\!\\]

@integer = $digit+
@floating = $digit+(\.$digit+)?
@comment = \#.*
@str = \"($digit|$letter|($white # \n)|($special # [\\\"])|\\\"|\\\\|\\n|\\t)*\"
@identifier = [a-z]($letter|$digit|_)*
@error = .

tokens :-
    $white+             ;
    @comment            ;
    \%                  {(pushToken $ Modex)} 
    \/                  {(pushToken $ Divex)}  
    \*                  {(pushToken $ Mult)}  
    \-                  {(pushToken $ Minus)}  
    \)                  {(pushToken $ CloseP)}  
    \(                  {(pushToken $ OpenP)}  
    \+                  {(pushToken $ Plus)}  
    \=                  {(pushToken $ Def)} 
    \;                  {(pushToken $ SemiColon)} 
    \,                  {(pushToken $ Comma)}
    \<                  {(pushToken $ Less)}
    \>                  {(pushToken $ More)} 
    not                 {(pushToken $ Not)}
    and                 {(pushToken $ And)}
    or                  {(pushToken $ Or)}
    \=\=                {(pushToken $ Eq)}
    \/\=                {(pushToken $ Neq)}
    \>\=                {(pushToken $ Moreq)}
    \<\=                {(pushToken $ Lesseq)}  
    div                 {(pushToken $ Div)}
    mod                 {(pushToken $ Mod)}
    \-\>                {(pushToken $ Arrow)} 
    number              {(pushToken $ Number)}
    boolean             {(pushToken $ Boolean)}
    true                {(pushToken $ True')}
    false               {(pushToken $ False')}
    with                {(pushToken $ With)}
    do                  {(pushToken $ Do)}
    end                 {(pushToken $ End)}
    if                  {(pushToken $ If)}
    then                {(pushToken $ Then)}
    else                {(pushToken $ Else)}
    while               {(pushToken $ While)}
    for                 {(pushToken $ For)}
    repeat              {(pushToken $ Repeat)}
    begin               {(pushToken $ Begin)}
    return              {(pushToken $ Return)}
    func                {(pushToken $ Func)}
    times               {(pushToken $ Times)}
    program             {(pushToken $ Program)}
    writeln             {(pushToken $ WriteLn)}
    write               {(pushToken $ Write)}
    by                  {(pushToken $ By)}
    from                {(pushToken $ From)}
    to                  {(pushToken $ To)}
    read                {(pushToken $ Read)}
    @integer            {(pushToken $ Integer) }
    @floating           {(pushToken $ Floating) }
    @str                {(pushToken $ Str) }
    @identifier         {(pushToken $ Identifier) }
    @error              {(pushToken $ LexError) }

{

alexEOF :: Alex ()
alexEOF = return ()

data Token = Integer AlexPosn String            |
             Floating AlexPosn String           | 
             Str AlexPosn String                |
             Identifier AlexPosn String         |
             LexError AlexPosn String           |
             Modex AlexPosn String              |
             Divex AlexPosn String              |
             Mult AlexPosn String               |
             Minus AlexPosn String              |
             CloseP AlexPosn String             |
             OpenP AlexPosn String              |
             Plus AlexPosn String               |
             Def AlexPosn String                |
             SemiColon AlexPosn String          |
             Comma AlexPosn String              |
             Less AlexPosn String               |
             More AlexPosn String               |
             Not AlexPosn String                |
             And AlexPosn String                |
             Or AlexPosn String                 |
             Eq AlexPosn String                 |
             Neq AlexPosn String                |
             Moreq AlexPosn String              |
             Lesseq AlexPosn String             |
             Div AlexPosn String                |
             Mod AlexPosn String                |
             Arrow AlexPosn String              |
             Number AlexPosn String             |
             Boolean AlexPosn String            |
             True' AlexPosn String              |
             False' AlexPosn String             |
             With AlexPosn String               |
             Do AlexPosn String                 |
             End AlexPosn String                |
             If AlexPosn String                 |
             Else AlexPosn String               |
             Then AlexPosn String               |
             While AlexPosn String              |
             For AlexPosn String                |
             Repeat AlexPosn String             |
             Begin AlexPosn String              |
             Return AlexPosn String             |
             Func AlexPosn String               |
             Times AlexPosn String              |
             Program AlexPosn String            |
             WriteLn AlexPosn String            |
             Write AlexPosn String              |
             By AlexPosn String                 |
             From AlexPosn String               |
             To AlexPosn String                 |
             Read AlexPosn String
             deriving (Eq)



data AlexUserState = AlexUserState 
                {
                    lexerError    :: Bool ,
                    lexerTokens   :: [Token]
                }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 
                {
                    lexerError    = True ,
                    lexerTokens   = []
                }


modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \sstate -> let current = alex_ust sstate
                                          new = f current
                                      in
                                          Right (sstate { alex_ust = new },())


getUserState ::  Alex AlexUserState
getUserState = Alex $ \state -> Right (state,alex_ust state)


isError :: Token -> Bool
isError (LexError _ _) = True
isError _ = False


pushToken :: (AlexPosn -> String -> Token) -> AlexAction ()
pushToken tokenizer =
    \(posn,prevChar,pending,str) len -> modifyUserState (push (take len str) posn) >> alexMonadScan
    where

        whatToPush :: [Token] -> Token -> [Token]
        whatToPush tks@( (LexError _ _) : _) tk@(LexError _ _) = tks++[tk]        -- Errors and new Error
        whatToPush tks@((LexError _ _) : _) _ = tks                                  -- Errors and new Normal Token
        whatToPush tks tk@(LexError _ _) = [tk]                                      -- Normal Tokens and new Error
        whatToPush tks tk = tks++[tk]                                                   -- Normal Tokens (might be empty) 
                                                                                       

        push :: String -> AlexPosn -> AlexUserState -> AlexUserState
        push st p ts = 
            ts{
                lexerTokens = whatToPush (lexerTokens ts) newToken ,
                lexerError = (lexerError ts) && not(isError newToken)
            }
            where 
                newToken = tokenizer p st
         
runAlexScan :: String -> Either String AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState         


}
