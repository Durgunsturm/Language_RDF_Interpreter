--Queries normalised turtle files
module Main where
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)

data RDFTerm = URI String | LitInt Int | LitStr String deriving (Show, Eq, Ord)
type Triple = (RDFTerm, RDFTerm, RDFTerm)
type Binding = [(String, RDFTerm)]
data Query = Select [String] [Condition] deriving Show
data Condition = Eq String RDFTerm | Gt String Int | Lt String Int deriving Show

-- separates line into 3 strings: subject, predicate, object
tokenise :: String -> [String]
tokenise [] = []
tokenise (' ':xs) = tokenise xs
tokenise ('\t':xs) = tokenise xs
tokenise ('"':xs) =
	let (str, rest) = break (== '"') xs
	in case rest of
		('"':rs) -> ("\"" ++ str ++ "\"") : tokenise rs
		_ -> ("\"" ++ str) : tokenise rest
tokenise ('<':xs) =
	let (str, rest) = break (== '>') xs
	in case rest of
		('>':rs) -> ("<" ++ str ++ ">") : tokenise rs
		_ -> ("<" ++ str) : tokenise rest
tokenise xs =
	let (token, rest) = break (\c -> c == ' ' || c == '\t') xs
	in token : tokenise rest

-- converts tokenised strings into RDFTerm values
parseTerm :: String -> RDFTerm
parseTerm ('<':xs) | not (null xs) && last xs == '>' = URI (init xs)
parseTerm ('"':xs) | not (null xs) && last xs == '"' = LitStr (init xs)
parseTerm str
	| not (null str) && all isDigit (if head str == '-' then tail str else str) = LitInt (read str)
	| otherwise = LitStr str

-- tokenises individual line and then converts elements into RDFTerms, returns a Maybe Triple to ensure a value is always returned
parseLine :: String -> Maybe Triple
parseLine line = 
	case filter (/= ".") (tokenise line) of
		[s, p, o] -> Just (parseTerm s, parseTerm p, parseTerm o)
		_ -> Nothing

-- parses each line individually, using Haskell 'lines' function to separate on '\n' characters in doc
parseRDF :: String -> [Triple]
parseRDF doc = mapMaybe parseLine (lines doc)

-- converts RDFTerm into string
unparseTerm :: RDFTerm -> String
unparseTerm (URI u) = "<" ++ u ++ ">"
unparseTerm (LitInt i) = show i
unparseTerm (LitStr s) = "\"" ++ s ++ "\""

-- combines Triple into single string
unparseTriple :: Triple -> String
unparseTriple (s, p, o) =
	unparseTerm s ++ " " ++ unparseTerm p ++ " " ++ unparseTerm o ++ " ."

-- unparses all triples and then combines them using unlines to insert '\n' between triple strings
unparseRDF :: [Triple] -> String
unparseRDF triples = unlines (map unparseTriple triples)

--lookup variable value in current binding
lookupVar :: String -> Binding -> Maybe RDFTerm
lookupVar = lookup

-- evaluate single condition against binding
evalCondition :: Condition -> Binding -> Bool
evalCondition (Eq var term) env =
	case lookupVar var env of 
		Just val -> val == term
		Nothing -> False
evalCondition (Gt var val) env =
	case lookupVar var env of
		Just (LitInt i) -> i > val
		_ -> False
evalCondition (Lt var val) env =
	case lookupVar var env of
		Just (LitInt i) -> i < val
		_ -> False

-- check binding satisfies all conditions in WHERE clause
evalConditions :: [Condition] -> Binding -> Bool
evalConditions cons env = all (`evalCondition` env) cons

-- project only variables requested in SELECT clause
project :: [String] -> Binding -> Binding
project vars env = [(v, t) | v <- vars, Just t <- [lookupVar v env]]

-- convert 3-variable binding back to RDF Triple for output
bindingToTriple :: Binding -> Maybe Triple
bindingToTriple env = do
	s <- lookupVar "?s" env
	p <- lookupVar "?p" env
	o <- lookupVar "?o" env
	return (s, p, o)

-- takes parsed program and RDF graph and then filters contents of RDF graph against conditions in program 
executeQuery :: Query -> [Triple] -> [Triple]
executeQuery (Select vars cons) db =
	let
		initialBindings = [[("?s", s), ("?p", p), ("?o", o)] | (s, p, o) <- db]
		filteredBindings = filter (evalConditions cons) initialBindings
		projectedBindings = map (project vars) filteredBindings
	in
		mapMaybe bindingToTriple projectedBindings

-- for testing, predefines generic content for RDF document, converts that String into list of Triple objects, predefines generic query in parsed format, runs executeQuery on predefined program and parsed triples, then outputs results as an unparsed string
main :: IO ()
main = do
	let 
		rdfData = "<http://data.org/Alice> <http://example.org/ont/hasAge> 25 .\n<http://data.org/Bob> <http://example.org/ont/hasAge> 19 .\n<http://data.org/Charlie> <http://example.org/ont/hasAge> 30 .\n<http://data.org/Alice> <http://example.org/ont/knows> <http://data.org/Bob> ."
		triples = parseRDF rdfData
		myQuery = Select ["?s", "?p", "?o"] [Gt "?o" 24]
		results = executeQuery myQuery triples
	putStrLn "Query Results (S, P, O format)" 
	putStr (unparseRDF results)