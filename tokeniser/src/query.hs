--Queries normalised turtle files
module Main where
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Data.List (nub,intersect)

type Dataset = [(String, [Triple])]
data RDFTerm = URI String | LitInt Int | LitStr String deriving (Show, Eq, Ord)
type GraphRef = Maybe String
type Triple = (RDFTerm, RDFTerm, RDFTerm)
type Binding = [(String, RDFTerm)]
data Query = Select [String] [String] [Condition] -- [input variables] [graph variables] [query conditions]
		   | Union Query Query
		   | Intersection Query Query
		   deriving Show
data Condition = 
	Eq String GraphRef RDFTerm 
	| Not String GraphRef RDFTerm
	| Gt String GraphRef Int 
	| Lt String GraphRef Int 
	| Gte String GraphRef Int
	| Lte String GraphRef Int
	| Max String GraphRef
	| Min String GraphRef
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

-- evaluate single condition against binding, utilise full dataset for aggregate functions
evalCondition :: Condition -> [Binding] -> Binding -> Bool
evalCondition (Eq var gRef term) _ env =
	case lookupVar (resolveVar var gRef) env of 
		Just val -> val == term
		Nothing -> False
evalCondition (Not var gRef term) _ env =
	case lookupVar (resolveVar var gRef) env of
		Just val -> val /= term
		Nothing -> False
evalCondition (Gt var gRef val) _ env =
	case lookupVar (resolveVar var gRef) env of
		Just (LitInt i) -> i > val
		_ -> False
evalCondition (Lt var gRef val) _ env =
	case lookupVar (resolveVar var gRef) env of
		Just (LitInt i) -> i < val
		_ -> False
evalCondition (Gte var gRef val) _ env =
	case lookupVar (resolveVar var gRef) env of
		Just (LitInt i) -> i >= val
		_ -> False
evalCondition (Lte var gRef val) _ env =
	case lookupVar (resolveVar var gRef) env of
		Just (LitInt i) -> i <= val
		_ -> False
evalCondition (Max var gRef) allEnvs env =
	let
		resolvedVar = resolveVar var gRef

		isTargetVar k = k == var || dropWhile (/= '.') k == "." ++ var
		
		groupingKey = filter (\(k, _) -> not (isTargetVar k)) env

		matchesGroup key targetEnv = all (\(k, v) -> lookupVar k targetEnv == Just v) key

		sameGroupVals = [val | e <- allEnvs, matchesGroup groupingKey e, Just val <- [lookupVar resolvedVar e]]
	in case lookupVar resolvedVar env of
		Just val -> not (null sameGroupVals) && val == maximum sameGroupVals
		Nothing -> False
evalCondition (Min var gRef) allEnvs env =
	let
		resolvedVar = resolveVar var gRef

		isTargetVar k = k == var || dropWhile (/= '.') k == "." ++ var
		
		groupingKey = filter (\(k, _) -> not (isTargetVar k)) env

		matchesGroup key targetEnv = all (\(k, v) -> lookupVar k targetEnv == Just v) key

		sameGroupVals = [val | e <- allEnvs, matchesGroup groupingKey e, Just val <- [lookupVar resolvedVar e]]
	in case lookupVar resolvedVar env of
		Just val -> not (null sameGroupVals) && val == minimum sameGroupVals
		Nothing -> False
evalCondition (And c1 c2) allEnvs env =
	evalCondition c1 allEnvs env && evalCondition c2 allEnvs env
evalCondition (Or c1 c2) allEnvs env =
	evalCondition c1 allEnvs env || evalCondition c2 allEnvs env

-- apply condition sequentially to execute filters before aggregate functions
applyConditions :: [Condition] -> [Binding] -> [Binding]
applyConditions cons initialEnvs =
	foldl (\envs cond -> filter (evalCondition cond envs) envs) initialEnvs cons

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
executeQuery (Select vars fromGraphs cons) ds =
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
		graphsData = [(g,ts) | g <- fromGraphs, Just ts <- [lookup g ds]]

		-- build list of bindings for each graph (first graph marked as True)
		buildGraphBindings :: [(String, [Triple])] -> [[Binding]]
		buildGraphBindings [] = []
		buildGraphBindings ((g1, ts1):rest) =
			map (bindTriple g1 True) ts1 : map (\(g,ts) -> map (bindTriple g False) ts) rest
		
		-- cartesian product of bindigns across all queried graphs
		combinations = sequence (buildGraphBindings graphsData)

		initialBindings = map concat combinations

		-- evaluate and project
		filteredBindings = applyConditions cons initialBindings
		projectedBindings = map (project vars) filteredBindings
		triples = mapMaybe bindingToTriple projectedBindings
	in nub triples
executeQuery (Union q1 q2) ds =
	let 
		results1 = executeQuery q1 ds
		results2 = executeQuery q2 ds
	in nub (results1 ++ results2)
executeQuery (Intersection q1 q2) ds =
	let
		results1 = executeQuery q1 ds
		results2 = executeQuery q2 ds
	in intersect results1 results2
		

-- for testing, predefines generic content for RDF document, converts that String into list of Triple objects, predefines generic query in parsed format, runs executeQuery on predefined program and parsed triples, then outputs results as an unparsed string
main :: IO ()
main = 
    do
        putStrLn "Task 1 results:"
        putStr (unparseRDF (executeQuery query1 dataset1))
        putStrLn "\nTask 2 results:"
        putStr (unparseRDF (executeQuery query2 dataset2))
        putStrLn "\nTask 3 results:"
        putStr (unparseRDF (executeQuery query3 dataset3))
		putStrLn "\nTask 4 results:"
		putStr (unparseRDF (executeQuery query4 dataset4))
		putStrLn "\nTask 5 results:"
	
    where 
        -- Task 1
	    rdfDataG11 = "<http://example.org/alice> <http://example.org/ont/name> \"Alice\" .\n<http://example.org/alice> <http://example.org/ont/worksFor> <http://example.org/uos> .\n<http://example.org/bob> <http://example.org/ont/name> \"Bob\" ."
	    triplesG11 = parseRDF rdfDataG11
	    rdfDataG21 = "<http://example.org/charlie> <http://example.org/ont/name> \"Charlie\" .\n<http://example.org/charlie> <http://example.org/ont/studiesAt> <http://example.org/uos> .\n<http://example.org/dave> <http://example.org/ont/name> \"Dave\" ."
	    triplesG21 = parseRDF rdfDataG21
	    dataset1 = [("?g1",triplesG11),("?g2",triplesG21)]
	    query1 = Union (Select ["?s","?p","?o"] ["?g1"] []) (Select ["?s","?p","?o"] ["?g2"] [])

	    -- Task 2
	    rdfData2 = "<http://example.org/alice> <http://example.org/ont/hasAge> 25 .\n<http://example.org/bob> <http://example.org/ont/hasAge> 19 .\n<http://example.org/charlie> <http://example.org/ont/hasAge> 30 .\n<http://example.org/dave> <http://example.org/ont/name> \"Dave\" ."
	    triples2 = parseRDF rdfData2
	    dataset2 = [("?g",triples2)]
	    query2 = Select ["?s","?p","?o"] ["?g"] [(Eq "?p" Nothing (URI "http://example.org/ont/hasAge")), (Gte "?o" Nothing 21)]
	
	    -- Task 3
	    rdfData3 = "<http://example.org/alice> <http://example.org/ont/studiesAt> <http://example.org/uos> .\n<http://example.org/bob> <http://example.org/ont/worksFor> <http://example.org/uos> .\n<http://example.org/charlie> <http://example.org/ont/studiesAt> <http://example.org/oxford> .\n<http://example.org/dave> <http://example.org/ont/worksFor> <http://example.org/google> .\n<http://example.org/eve> <http://example.org/ont/name> \"Eve\" ."
	    triples3 = parseRDF rdfData3
	    dataset3 = [("?g",triples3)]
	    query3 = Select ["?s","?p","?o"] ["?g"] [And (Or (Eq "?p" Nothing (URI "http://example.org/ont/studiesAt")) (Eq "?p" Nothing (URI "http://example.org/ont/worksFor"))) (Eq "?o" Nothing (URI "http://example.org/uos"))]

		-- Task 4
		rdfData4 = "<http://example.org/alice> <http://example.org/ont/price> 100 .\n<http://example.org/alice> <http://example.org/ont/price> 200 .\n<http://example.org/bob> <http://example.org/ont/price> 50 .\n<http://example.org/bob> <http://example.org/ont/price> 150 .\n<http://example.org/charlie> <http://example.org/ont/name> \"Charlie\" ."
		triples4 = parseRDF rdfData4
		dataset4 = [("?quux",triples4)]
		query4 = Select ["?s","?p","?o"] ["?quux"] [Eq "?p" Nothing (URI "http://example.org/ont/price"), Max "?o" Nothing]

		-- Task 5