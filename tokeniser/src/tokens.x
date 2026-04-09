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
    "INTER" { \p s -> TokenInter p }
    "DIFF"  { \p s -> TokenDiff p }
    "MAX"   { \p s -> TokenMax p }
    "MIN"   { \p s -> TokenMin p }
    "init"  { \p s -> TokenNorm p }
    "COUNT" { \p s -> TokenCount p }
    "SUM" { \p s -> TokenSum p }
    "AVG" { \p s -> TokenAvg p }
    "SAMPLE" { \p s -> TokenSample p }
    "GROUP_CONCAT" { \p s -> TokenGroupConcat p }
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
    \" [^\"]+ \" { \p s -> TokenStr p (init (tail s)) } -- Removes encasing quotes from string literal
    "^<" [^ \<\>]+ ">" { \p s -> TokenURI p (tail (init (tail s))) } -- Removes the leading ! from the URI


-- Haskell data types    
{
data Token = TokenSelect AlexPosn
           | TokenFrom AlexPosn
           | TokenWhere AlexPosn
           | TokenTo AlexPosn
           | TokenUnion AlexPosn
           | TokenGroup AlexPosn
           | TokenInter AlexPosn
           | TokenDiff AlexPosn
           | TokenMax AlexPosn
           | TokenMin AlexPosn
           | TokenNorm AlexPosn
           | TokenCount AlexPosn
           | TokenSum AlexPosn
           | TokenAvg AlexPosn
           | TokenSample AlexPosn
           | TokenGroupConcat AlexPosn
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
           | TokenStr AlexPosn String
           deriving (Eq, Show)

tokenPosn :: Token -> AlexPosn
tokenPosn t = case t of
    TokenSelect p -> p
    TokenFrom p -> p
    TokenWhere p -> p
    TokenTo p -> p
    TokenUnion p -> p
    TokenGroup p -> p
    TokenInter p -> p
    TokenDiff p -> p
    TokenMax p -> p
    TokenMin p -> p
    TokenNorm p -> p
    TokenCount p -> p
    TokenSum p -> p
    TokenAvg p -> p
    TokenSample p -> p
    TokenGroupConcat p -> p
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
    TokenStr p _ -> p
}