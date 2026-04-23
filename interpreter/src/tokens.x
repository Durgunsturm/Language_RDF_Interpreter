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
    \? $alpha [$alpha $digit]* (\. \? $alpha [$alpha $digit]*)* { \p s -> TokenVar p s }
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

tokenPosn :: Token -> String
tokenPosn (TokenSelect (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFrom (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhere (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTo (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenUnion (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGroup (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInter (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiff (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMax (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMin (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNorm (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCount (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSum (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAvg (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSample (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGroupConcat (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNumber (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThanOrEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThanOrEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParentheses (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParentheses (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLCurly (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRCurly (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLineEnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenURI (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStr (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
}