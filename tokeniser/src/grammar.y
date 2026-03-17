{ 
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token } 
%error { parseError }
%token 
    SELECT              { TokenSelect _ }
    FROM                { TokenFrom _ }
    WHERE               { TokenWhere _ }
    TO                  { TokenTo _ }
    UNION               { TokenUnion _ }
    GROUP               { TokenGroup _ }
    MAX                 { TokenMax _ }
    MIN                 { TokenMin _ }
    ','                 { TokenComma _ }
    int                 { TokenNumber _ $$ }
    in                  { TokenIn _ }
    "&&"                { TokenAnd _ }
    "||"                { TokenOr _ }
    "<="                { TokenLessThanOrEquals _ }
    ">="                { TokenGreaterThanOrEquals _ }
    '!'                 { TokenNot _ }
    '='                 { TokenEquals _ }
    '<'                 { TokenLessThan _ }
    '>'                 { TokenGreaterThan _ }
    '('                 { TokenLParentheses _ }
    ')'                 { TokenRParentheses _ }
    '{'                 { TokenLCurly _ }
    '}'                 { TokenRCurly _ }
    '.'                 { TokenLineEnd _ }
    var                 { TokenVar _ $$ }
    uri                 { TokenURI _ $$ }

%right '!'
%left "||"
%left "&&"
%% 

Exp             : SelectClause FromClause ToClause WhereClause      { Query $1 $2 $3 $4 }

VarList         : var                                               { [$1] }
                | VarList var                                       { $1 ++ [$2] }

ConditionClause : var '=' uri '.'                                   { UriCond $1 $3 }
                | var '=' int '.'                                   { IntCond $1 $3 }

Conditions      : ConditionClause                                   { [$1] }
                | Conditions ConditionClause                        { $1 ++ [$2] }

WhereClause     : WHERE '{' Conditions '}'                          { $3 }

FromClause      : FROM '(' VarList ')'                              { $3 }

ToClause        : TO '(' VarList ')'                                { $3 }

SelectClause    : SELECT '(' VarList ')'                            { $3 }

{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ showPosn (tokenPosn t))

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ":" ++ show col

type Var                        = String

type VarList                    = [Var]

data ConditionClause            = UriCond Var String
                                | IntCond Var Int
                                deriving Show

type Conditions                 = [ConditionClause]

type WhereClause                = Conditions

type ToClause                   = [String]

type FromClause                 = [String]

type SelectClause               = [String]

data Exp                        = Query SelectClause FromClause ToClause WhereClause
                                deriving Show
}