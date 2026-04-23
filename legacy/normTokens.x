{ 
module NormTokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
  $white+               ; 
  "--".*                ; 
  "@base"               { \p s -> TokenBase p} 
  "@prefix"             { \p s -> TokenPrefix p }
  "<" [^ \<\>]+ ">"     { \p s -> TokenURI p s }
  [^ \.\:\,]+           { \p s -> TokenVal p s }
  ":"                   { \p s -> TokenColon p }
  ","                   { \p s -> TokenComma p }
  ";"                   { \p s -> TokenSemi p }
  "."                   { \p s -> TokenDot p }

{ 
-- Each action has type :: AlexPosn -> String -> Token 

-- The token type: 
data NormToken = 
  TokenBase AlexPosn        | 
  TokenPrefix  AlexPosn     | 
  TokenURI AlexPosn String  |
  TokenVal AlexPosn String  | 
  TokenDot AlexPosn         |
  TokenColon AlexPosn       |
  TokenComma AlexPosn       |
  TokenSemi AlexPosn
  deriving (Eq,Show) 

tokenPosn :: NormToken -> String
tokenPosn (TokenBase (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPrefix  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenURI  (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVal  (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDot  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenColon  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemi  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

}