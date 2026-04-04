{ 
module NormGrammar where
import NormTokens
}

%name parseCalc
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

Exp         : TTLine                    { [$1] }
            | TTLine Exp                { $1 : $2 }

TTLine      : uri uri Obj '.'           { Whole $1 $2 $3 }
            | base uri '.'              { Base $2 }
            | prefix val ':' uri '.'    { Prefix $2 $4 }
            | uri val ':' val Obj ',' Obj ';' val ':' val Obj '.'

Obj         : uri                       { URI $1 }
            | val                       { Val $1 }

