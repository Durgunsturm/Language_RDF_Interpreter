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
    COUNT               { TokenCount _ }
    AVG                 { TokenAvg _ }
    SUM                 { TokenSum _ }
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
    str                 { TokenStr _ $$ }

%right '!'
%left "||"
%left "&&"
%left UNION GROUP INTER DIFF
%nonassoc '=' '<' '>' "<=" ">="
%% 

Exp             : Expr                                              { [$1] }
                | Expr Exp                                          { $1 : $2 }

Expr            : Query                                             { Queries $1 }
                | var '=' RDFTerm '.'                               { Variable $1 $3 }
                | var '=' NORM var '.'                              { Norm $1 $4 $4 }
                | var '=' NORM var var '.'                          { Norm $1 $4 $5 }

Query           : SelectClause FromClause ToClause WhereClause      { Select $1 $2 (Just $3) (Just $4) }
                | SelectClause FromClause WhereClause               { Select $1 $2 Nothing (Just $3) } -- Output to console
                | SelectClause FromClause ToClause                  { Select $1 $2 (Just $3) Nothing } -- No where clause
                | SelectClause FromClause                          { Select $1 $2 Nothing Nothing }
                | '(' Query UNION Query ')'                         { Union $2 $4 }
                | '(' Query GROUP Query ')'                         { Group $2 $4 }
                | '(' Query INTER Query ')'                         { Inter $2 $4 }
                | '(' Query DIFF Query ')'                          { Diff $2 $4 }


-- Handles graph reference if declared, ignores it otherwise
GraphRef        : in var                                            { (Just $2) }
                |                                                   { Nothing }

Operand         : var GraphRef                                      { Var $1 $2 }
                | RDFTerm                                           { Const $1 }

RDFTerm         : uri                                               { URI $1 }
                | int                                               { LitInt $1 }
                | str                                               { LitStr $1 }


-- Handles variables separated by spaces or commas (and a mix of them technically)
VarList         : var                                               { [$1] }
                | var VarList                                       { $1 : $2 }
                | var ',' VarList                                   { $1 : $3 }

Condition       : '!' Condition                                     { Not $2 }
                | '(' Condition ')'                                 { $2 }
                | Condition "&&" Condition                          { And $1 $3 }
                | Condition "||" Condition                          { Or $1 $3 }
                | Operand '=' Operand                               { Eq $1 $3 }
                | Operand '<' Operand                               { Lt $1 $3 }
                | Operand '>' Operand                               { Gt $1 $3 }
                | Operand "<=" Operand                              { LtEq $1 $3 }
                | Operand ">=" Operand                              { GtEq $1 $3 }
                | MAX var GraphRef                                  { Max $2 $3 }
                | MIN var GraphRef                                  { Min $2 $3 }
                | COUNT var GraphRef                                { Count $2 $3 }
                | AVG var GraphRef                                  { Avg $2 $3 }
                | SUM var GraphRef                                  { Sum $2 $3 }

ConditionLine   : Condition '.'                                     { $1 }

Conditions      : ConditionLine                                     { [$1] }
                | ConditionLine Conditions                          { $1 : $2 }


{- Handles query syntax -}

WhereClause     : WHERE '{' Conditions '}'                          { $3 }

FromClause      : FROM '(' VarList ')'                              { $3 }

ToClause        : TO '(' VarList ')'                                { $3 }

SelectClause    : SELECT '(' VarList ')'                            { $3 }

{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ tokenPosn t)

-- Optional reference to a specific graph
type GraphRef                   = Maybe String

-- Operand used as comparators in conditions
data Operand                    = Var String GraphRef
                                | Const RDFTerm 
                                deriving Show

-- Terms that hold values
data RDFTerm                    = URI String
                                | LitInt Int
                                | LitStr String
                                deriving Show

-- List of strings holding variable names
type Var                        = String
type VarList                    = [Var]

data Condition                  = Not Condition
                                | And Condition Condition
                                | Or Condition Condition
                                | Eq Operand Operand
                                | Lt Operand Operand
                                | Gt Operand Operand
                                | LtEq Operand Operand
                                | GtEq Operand Operand
                                | Max String GraphRef
                                | Min String GraphRef
                                | Count String GraphRef
                                | Avg String GraphRef
                                | Sum String GraphRef
                                deriving Show

-- Type to separate conditions by line
type ConditionLine              = Condition
-- List of all conditions within a single where clause
type Conditions                 = [ConditionLine]


{- Parts a single query -}

type WhereClause                = Conditions

type ToClause                   = VarList

type FromClause                 = VarList

type SelectClause               = VarList

-- Handles a complex query with a single output
data Query                      = Select SelectClause FromClause (Maybe ToClause) (Maybe WhereClause)
                                | Union Query Query
                                | Group Query Query
                                | Inter Query Query
                                | Diff Query Query
                                deriving Show

-- Top level statements in code
data Expr                       = Queries Query
                                | Variable String RDFTerm
                                | Norm String String String
                                deriving Show

type Exp                        = [Expr]
}