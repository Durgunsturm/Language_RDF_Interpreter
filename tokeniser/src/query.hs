--Queries normalised turtle files
module Main where
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Data.List (nub)

type Dataset = [(String, [Triple])]
data RDFTerm = URI String | LitInt Int | LitStr String deriving (Show, Eq, Ord)
type GraphRef = Maybe String
type Triple = (RDFTerm, RDFTerm, RDFTerm)
type Binding = [(String, RDFTerm)]
data Query = Select [String] [String] [Condition] deriving Show
data Condition = 
	Eq String GraphRef RDFTerm 
	| Not String GraphRef RDFTerm
	| Gt String GraphRef Int 
	| Lt String GraphRef Int 
	| Gte String GraphRef Int
	| Lte String GraphRef Int
	| And Condition Condition
	| Or Condition Condition
	deriving Show

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

-- resolve variable name based on explicity/implicit graph scopes
resolveVar :: String -> GraphRef -> String
resolveVar var Nothing = var
resolveVar var (Just g) = g ++ "." ++ var

-- evaluate single condition against binding
evalCondition :: Condition -> Binding -> Bool
evalCondition (Eq var gRef term) env =
	case lookupVar (resolveVar var gRef) env of 
		Just val -> val == term
		Nothing -> False
evalCondition (Not var gRef term) env =
	case lookupVar (resolveVar var gRef) env of
		Just val -> val /= term
		Nothing -> False
evalCondition (Gt var gRef val) env =
	case lookupVar (resolveVar var gRef) env of
		Just (LitInt i) -> i > val
		_ -> False
evalCondition (Lt var gRef val) env =
	case lookupVar (resolveVar var gRef) env of
		Just (LitInt i) -> i < val
		_ -> False
evalCondition (Gte var gRef val) env =
	case lookupVar (resolveVar var gRef) env of
		Just (LitInt i) -> i >= val
		_ -> False
evalCondition (Lte var gRef val) env =
	case lookupVar (resolveVar var gRef) env of
		Just (LitInt i) -> i <= val
		_ -> False
evalCondition (And c1 c2) env =
	evalCondition c1 env && evalCondition c2 env
evalCondition (Or c1 c2) env =
	evalCondition c1 env || evalCondition c2 env

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
executeQuery :: Query -> Dataset -> [Triple]
executeQuery (Select vars fromGraphs cons) db =
	let
		-- creates bindings for triple, primary graph is implicit
		bindTriple :: String -> Bool -> Triple -> Binding
		bindTriple gName isPrimary (s,p,o) =
			let 
				explicitBinds = [
								(gName ++ ".?s",s)
								, (gName ++ ".?p",p)
								, (gName ++ ".?o",o)
								]
				implicitBinds = if isPrimary 
								then [("?s",s),("?p",p),("?o",o)] 
								else []
			in explicitBinds ++ implicitBinds

		-- extract specified graphs from dataset
		graphsData = [(g,ts) | g <- fromGraphs, Just ts <- [lookup g db]]

		-- build list of bindings for each graph (first graph marked as True)
		buildGraphBindings :: [(String, [Triple])] -> [[Binding]]
		buildGraphBindings [] = []
		buildGraphBindings ((g1, ts1):rest) =
			map (bindTriple g1 True) ts1 : map (\(g,ts) -> map (bindTriple g False) ts) rest
		
		-- cartesian product of bindigns across all queried graphs
		combinations = sequence (buildGraphBindings graphsData)

		initialBindings = map concat combinations

		-- evaluate and project
		filteredBindings = filter (evalConditions cons) initialBindings
		projectedBindings = map (project vars) filteredBindings
		triples = mapMaybe bindingToTriple projectedBindings
	in nub triples
		

-- for testing, predefines generic content for RDF document, converts that String into list of Triple objects, predefines generic query in parsed format, runs executeQuery on predefined program and parsed triples, then outputs results as an unparsed string
main :: IO ()
main = do
	let 
		rdfData1 = "<http://data.org/Alice> <http://example.org/ont/hasAge> 25 .\n<http://data.org/Bob> <http://example.org/ont/hasAge> 19 ."
		triples1 = parseRDF rdfData1
		rdfData2 = "<http://data.org/Charlie> <http://example.org/ont/hasAge> 30 .\n<http://data.org/Alice> <http://example.org/ont/knows> <http://data.org/Bob> ."
		triples2 = parseRDF rdfData2
		dataset = [("?g1",triples1),("?g2",triples2)]
		query1 = Select ["?s","?p","?o"] ["?g1"]
			[And
				(Eq "?p" Nothing (URI "http://example.org/ont/hasAge"))
				(Gte "?o" Nothing 21)
			]
		query2 = Select ["?s","?p","?o"] ["?g1","?g2"]
			[And
				(Eq "?p" (Just "?g1") (URI "http://example.org/ont/hasAge"))
				(Gte "?o" Nothing 21)
			]
	putStrLn "Query 1 results:"
	putStr (unparseRDF (executeQuery query1 dataset))
	
	putStrLn "\nQuery 2 results:"
	putStr (unparseRDF (executeQuery query2 dataset))
