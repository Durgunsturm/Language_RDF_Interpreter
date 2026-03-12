{
module Tokens where
}

-- The basic wrapper takes a string and returns a list of tokens
%wrapper "basic"
$digit = 0-9
$alpha = [a-zA-Z]

-- The rules section
tokens :-
    $white+ ;
    "//" [^ \n]* ;
    "SELECT" { \s -> TokenSelect }
    "FROM" { \s -> TokenFrom }
    "WHERE" { \s -> TokenWhere }
    $digit+ { \s -> TokenNumber (read s) }
    "in" { \s -> TokenIn }
    "&&" { \s -> TokenAnd }
    "||" { \s -> TokenOr }
    "<=" { \s -> TokenLessThanOrEquals }
    ">=" { \s -> TokenGreaterThanOrEquals }
    "!" { \s -> TokenNot }
    "=" { \s -> TokenEquals }
    "<" { \s -> TokenLessThan }
    ">" { \s -> TokenGreaterThan }
    "(" { \s -> TokenLParentheses }
    ")" { \s -> TokenRParentheses }
    "{" { \s -> TokenLCurly }
    "}" { \s -> TokenRCurly }
    "." { \s -> TokenLineEnd }
    \? $alpha [$alpha $digit]* { \s -> TokenVar s }
    "<" [^\>]+ ">" { \s -> TokenURI s }

-- Haskell data types    
{
data Token = TokenSelect
           | TokenFrom
           | TokenWhere
           | TokenIn
           | TokenAnd
           | TokenOr
           | TokenLessThanOrEquals
           | TokenGreaterThanOrEquals
           | TokenNot
           | TokenEquals
           | TokenLessThan
           | TokenGreaterThan
           | TokenLParentheses
           | TokenRParentheses
           | TokenLCurly
           | TokenRCurly
           | TokenLineEnd
           | TokenVar String
           | TokenURI String
           | TokenNumber Int
           deriving (Eq, Show)
}