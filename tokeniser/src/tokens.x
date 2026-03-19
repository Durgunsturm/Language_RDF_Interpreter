{
module Tokens where
}

-- Using posn wrapper so that we can get line numbers and column numbers for each token
%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]

-- The rules section
tokens :-
    $white+ ;
    "//" [^ \n]* ;
    "SELECT" { \p s -> TokenSelect p }
    "FROM" { \p s -> TokenFrom p }
    "WHERE" { \p s -> TokenWhere p }
    "TO" { \p s -> TokenTo p }
    "UNION" { \p s -> TokenUnion p }
    "GROUP" { \p s -> TokenGroup p }
    "MAX"   { \p s -> TokenMax p }
    "MIN"   { \p s -> TokenMin p }
    "," { \p s -> TokenComma p }
    $digit+ { \p s -> TokenNumber p (read s) }
    "in" { \p s -> TokenIn p }
    "&&" { \p s -> TokenAnd p }
    "||" { \p s -> TokenOr p }
    "<=" { \p s -> TokenLessThanOrEquals p }
    ">=" { \p s -> TokenGreaterThanOrEquals p }
    "!" { \p s -> TokenNot p }
    "=" { \p s -> TokenEquals p }
    "<" { \p s -> TokenLessThan p }
    ">" { \p s -> TokenGreaterThan p }
    "(" { \p s -> TokenLParentheses p }
    ")" { \p s -> TokenRParentheses p }
    "{" { \p s -> TokenLCurly p }
    "}" { \p s -> TokenRCurly p }
    "." { \p s -> TokenLineEnd p }
    \? $alpha [$alpha $digit]* { \p s -> TokenVar p s }
    "<" [^ \>]+ ">" { \p s -> TokenURI p s }

-- Haskell data types    
{
data Token = TokenSelect AlexPosn
           | TokenFrom AlexPosn
           | TokenWhere AlexPosn
           | TokenTo AlexPosn
           | TokenUnion AlexPosn
           | TokenGroup AlexPosn
           | TokenMax AlexPosn
           | TokenMin AlexPosn
           | TokenComma AlexPosn
           | TokenNumber AlexPosn Int
           | TokenIn AlexPosn
           | TokenAnd AlexPosn
           | TokenOr AlexPosn
           | TokenLessThanOrEquals AlexPosn
           | TokenGreaterThanOrEquals AlexPosn
           | TokenNot AlexPosn
           | TokenEquals AlexPosn
           | TokenLessThan AlexPosn
           | TokenGreaterThan AlexPosn
           | TokenLParentheses AlexPosn
           | TokenRParentheses AlexPosn
           | TokenLCurly AlexPosn
           | TokenRCurly AlexPosn
           | TokenLineEnd AlexPosn
           | TokenVar AlexPosn String
           | TokenURI AlexPosn String
           deriving (Eq, Show)

tokenPosn :: Token -> AlexPosn
tokenPosn t = case t of
    TokenSelect p -> p
    TokenFrom p -> p
    TokenWhere p -> p
    TokenTo p -> p
    TokenUnion p -> p
    TokenGroup p -> p
    TokenMax p -> p
    TokenMin p -> p
    TokenComma p -> p
    TokenNumber p _ -> p
    TokenIn p -> p
    TokenAnd p -> p
    TokenOr p -> p
    TokenLessThanOrEquals p -> p
    TokenGreaterThanOrEquals p -> p
    TokenNot p -> p
    TokenEquals p -> p
    TokenLessThan p -> p
    TokenGreaterThan p -> p
    TokenLParentheses p -> p
    TokenRParentheses p -> p
    TokenLCurly p -> p
    TokenRCurly p -> p
    TokenLineEnd p -> p
    TokenVar p _ -> p
    TokenURI p _ -> p
}