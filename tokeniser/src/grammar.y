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
    INTER               { TokenInter _ }
    DIFF                { TokenDiff _ }
    MAX                 { TokenMax _ }
    MIN                 { TokenMin _ }
    NORM                { TokenNorm _ }
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
%left UNION GROUP INTER DIFF
%nonassoc '=' '<' '>' "<=" ">="
%% 

Exp             : Expr                                              { [$1] }
                | Expr Exp                                          { $1 : $2 }

Expr            : SingleQuery                                       { Sing $1 }
                | '(' Expr UNION Expr ')'                           { Union $2 $4 }
                | '(' Expr GROUP Expr ')'                           { Group $2 $4 }
                | '(' Expr INTER Expr ')'                           { Inter $2 $4 }
                | '(' Expr DIFF Expr ')'                            { Diff $2 $4 }
                | var '=' ConditionVal '.'                          { Variable $1 $3 }
                | var '=' NORM var '.'                              { Norm $1 $4 $4 }
                | var '=' NORM var var '.'                          { Norm $1 $4 $5 }

SingleQuery     : SelectClause FromClause ToClause WhereClause      { Selector $1 $2 $3 $4 }

ConditionVal    : uri                                               { UriCond $1 }
                | int                                               { IntCond $1 }

VarList         : var                                               { [$1] }
                | VarList var                                       { $1 ++ [$2] }

ConditionClause : '!' ConditionClause                               { Not $2 }
                | '(' ConditionClause ')'                           { $2 }
                | ConditionClause "&&" ConditionClause              { And $1 $3 }
                | ConditionClause "||" ConditionClause              { Or $1 $3 }
                | var '=' ConditionVal                              { Eq $1 $3 }
                | var '<' ConditionVal                              { Lt $1 $3 }
                | var '>' ConditionVal                              { Gt $1 $3 }
                | var "<=" ConditionVal                             { LtEq $1 $3 }
                | var ">=" ConditionVal                             { GtEq $1 $3 }

ConditionLine   : ConditionClause '.'                               { $1 }

Conditions      : ConditionLine                                     { [$1] }
                | Conditions ConditionLine                          { $1 ++ [$2] }

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

data ConditionVal               = UriCond String
                                | IntCond Int
                                deriving Show

type Var                        = String

type VarList                    = [Var]

data ConditionClause            = Not ConditionClause
                                | And ConditionClause ConditionClause
                                | Or ConditionClause ConditionClause
                                | Eq Var ConditionVal
                                | Lt Var ConditionVal
                                | Gt Var ConditionVal
                                | LtEq Var ConditionVal
                                | GtEq Var ConditionVal
                                deriving Show

type ConditionLine              = ConditionClause
type Conditions                 = [ConditionLine]


type WhereClause                = Conditions

type ToClause                   = [String]

type FromClause                 = [String]

type SelectClause               = [String]

data SingleQuery                = Selector SelectClause FromClause ToClause WhereClause
                                deriving Show

data Expr                       = Sing SingleQuery
                                | Union Expr Expr
                                | Group Expr Expr
                                | Inter Expr Expr
                                | Diff Expr Expr
                                | Variable String ConditionVal
                                | Norm String String String
                                deriving Show

type Exp                        = [Expr]
}