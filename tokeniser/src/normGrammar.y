{ 
module NormGrammar where
import NormTokens
}

%name parseTTL
%tokentype { NormToken } 
%error { parseError }
%token 
    base              { TokenBase _ }
    prefix            { TokenPrefix _ }
    uri               { TokenURI _ $$ }
    val               { TokenVal _ $$ }
    '.'               { TokenDot _ }
    ':'               { TokenColon _ }
    ','               { TokenComma _ }
    ';'               { TokenSemi _ }

%% 

Exp         : TTLine                                    { [$1] }
            | TTLine Exp                                { $1 : $2 }

TTLine      : uri uri StringVal '.'                     { Whole $1 $2 $3 }
            | base uri '.'                              { Ba $2 }
            | prefix val ':' uri '.'                    { Pre $2 $4 }
            | uri MatchBase '.'                         { Subj $1 $2 }

StringVal   : uri                                       { URI $1 }
            | val                                       { Val $1 }

MatchPred   : StringVal                                 { [$1] }
            | StringVal ',' MatchPred                   { $1 : $2 }

MatchBase   : val ':' val MatchPred                     { [( $1, $3, $4 )] }
            | val ':' val MatchPred ';' MatchBase       { ( $1, $3, $4 ) : $5 }

{ 
parseError :: [NormToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

type Exp = [TTLine]

data TTLine = Whole String String StringVal 
            | Ba String 
            | Pre String String 
            | Subj String MatchBase
            deriving Show

type MatchBase = [(String, String, MatchPred)]

type MatchPred = [StringVal]

data StringVal = URI String 
         | Val String
         deriving Show

}