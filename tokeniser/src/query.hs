--Queries normalised turtle files
module Query (
	parseRDF,
	unparseRDF,
	executeQuery,
	Dataset,
	Triple,
	Binding
) where
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Data.List (nub,intersect)
import Grammar

type Dataset = [(String, [Triple])] -- List of graph names and associated triples (parsed from input graphs)
type Triple = (RDFTerm, RDFTerm, RDFTerm) -- parsed version of the RDF graphs stored in a datatype that allows for easy manipulation in condition matching
type Binding = [(String, RDFTerm)] -- list of variable name ("?s", "?p", "?o", "?graph1.?s", etc...) and associated RDFTerm
instance Eq RDFTerm where
	(URI u1) == (URI u2) = u1 == u2
	(LitInt i1) == (LitInt i2) = i1 == i2
	(LitStr s1) == (LitStr s2) = s1 == s2
	_ == _ = False

instance Ord RDFTerm where
	compare (LitInt i1) (LitInt i2) = compare i1 i2
	compare (LitStr s1) (LitStr s2) = compare s1 s2
	compare (URI u1) (URI u2) = compare u1 u2
	compare (LitInt _) _ = LT
	compare _ (LitInt _) = GT
	compare (LitStr _) _ = LT
	compare _ (LitStr _) = GT

-- separates line into 3 strings: subject, predicate, object
tokenise :: String -> [String]
tokenise [] = []
tokenise (' ':xs) = tokenise xs -- ignore whitespace at head of String
tokenise ('\t':xs) = tokenise xs -- ignore \t characters at head of String
tokenise ('"':xs) = -- for string in RDF graph
	let (str, rest) = break (== '"') xs -- take all characters from first '"' to next '"'
	in case rest of
		('"':rs) -> ("\"" ++ str ++ "\"") : tokenise rs
		_ -> ("\"" ++ str) : tokenise rest
tokenise ('<':xs) = -- for URI in RDF graph
	let (str, rest) = break (== '>') xs -- take all characters from first '<' to next '>'
	in case rest of
		('>':rs) -> ("<" ++ str ++ ">") : tokenise rs
		_ -> ("<" ++ str) : tokenise rest
tokenise xs = -- otherwise (distinguish )
	let (token, rest) = break (\c -> c == ' ' || c == '\t') xs
	in token : tokenise rest

-- converts tokenised strings into RDFTerm values
parseTerm :: String -> RDFTerm
parseTerm ('<':xs) | not (null xs) && last xs == '>' = URI (init xs) -- when xs not null and final xs character is >, produce a URI RDF term
parseTerm ('"':xs) | not (null xs) && last xs == '"' = LitStr (init xs) -- when xs is not null and final xs character is ", produce a String RDF term
parseTerm str -- otherwise
	| not (null str) && all isDigit (if head str == '-' then tail str else str) = LitInt (read str) -- if characters in term are integers, produce an Integer RDF term
	| otherwise = LitStr str -- default to string when all other options fail

-- tokenises individual line and then converts elements into RDFTerms, returns a Maybe Triple to ensure a value is always returned
parseLine :: String -> Maybe Triple
parseLine line = 
	case filter (/= ".") (tokenise line) of -- filter out '.' characters
		[s, p, o] -> Just (parseTerm s, parseTerm p, parseTerm o) -- Maybe Triple containing parsed terms for subject, predicate,and object
		_ -> Nothing

-- parses each line individually, using Haskell 'lines' function to separate on '\n' characters in doc
parseRDF :: String -> [Triple]
parseRDF doc = mapMaybe parseLine (lines doc) -- parse lines of RDF graph into [Triple]

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

-- resolve operand to maybe RDFTerm
evalOperand :: Operand -> Binding -> Maybe RDFTerm
evalOperand (Const term) _ = Just term
evalOperand (Var var gRef) env = lookupVar (resolveVar var gRef) env

-- evaluate single condition against binding, utilise full dataset for aggregate functions
evalCondition :: Condition -> [Binding] -> Binding -> Bool
evalCondition (Eq op1 op2) _ env = -- equality condition
	case (evalOperand op1 env, evalOperand op2 env) of
		(Just val1, Just val2) -> val1 == val2 -- true only when values are equal
		_ -> False -- otherwise condition fails

evalCondition (Gt op1 op2) _ env =
	case (evalOperand op1 env, evalOperand op2 env) of
		(Just (LitInt i1), Just (LitInt i2)) -> i1 > i2 -- values are integers and i1 > i2
		_ -> False -- otherwise condition fails
evalCondition (Lt op1 op2) _ env =
	case (evalOperand op1 env, evalOperand op2 env) of
		(Just (LitInt i1), Just (LitInt i2)) -> i1 < i2 -- values are integers and i1 < i2
		_ -> False -- otherwise condition fails
evalCondition (GtEq op1 op2) _ env =
	case (evalOperand op1 env, evalOperand op2 env) of
		(Just (LitInt i1), Just (LitInt i2)) -> i1 >= i2 -- values are integers and i1 >= i2
		_ -> False -- otherwise condition fails
evalCondition (LtEq op1 op2) _ env =
	case (evalOperand op1 env, evalOperand op2 env) of
		(Just (LitInt i1), Just (LitInt i2)) -> i1 <= i2 -- values are integers and i1 <= i2
		_ -> False -- otherwise condition fails
evalCondition (Max var gRef) allEnvs env =
	let
		resolvedVar = resolveVar var gRef -- resolve variable
		isTargetVar k = k == var || dropWhile (/= '.') k == "." ++ var 
		groupingKey = filter (\(k, _) -> not (isTargetVar k)) env -- filter triples in binding to only match against target variable
		matchesGroup key targetEnv = all (\(k, v) -> lookupVar k targetEnv == Just v) key
		sameGroupVals = [val | e <- allEnvs, matchesGroup groupingKey e, Just val <- [lookupVar resolvedVar e]] 
	in case lookupVar resolvedVar env of
		Just val -> not (null sameGroupVals) && val == maximum sameGroupVals -- maximum can fetch more than one valid triple, assuming those triples don't match on at least one other aspect (e.g. maximise object, if subject in two max triples is different output both triples)
		Nothing -> False -- otherwise condition fails
evalCondition (Min var gRef) allEnvs env = -- same logic as maximise, just replace Just val condition in return with minimum instead of maximum
	let
		resolvedVar = resolveVar var gRef
		isTargetVar k = k == var || dropWhile (/= '.') k == "." ++ var
		groupingKey = filter (\(k, _) -> not (isTargetVar k)) env
		matchesGroup key targetEnv = all (\(k, v) -> lookupVar k targetEnv == Just v) key
		sameGroupVals = [val | e <- allEnvs, matchesGroup groupingKey e, Just val <- [lookupVar resolvedVar e]]
	in case lookupVar resolvedVar env of
		Just val -> not (null sameGroupVals) && val == minimum sameGroupVals -- minimum can fetch more than one valid triple, assuming those triples don't match on at least one other aspect (e.g. minimise object, if subject in two min triples is different output both triples)
		Nothing -> False -- otherwise condition fails
evalCondition (Not c) allEnvs env =
	not (evalCondition c allEnvs env)
evalCondition (And c1 c2) allEnvs env =
	evalCondition c1 allEnvs env && evalCondition c2 allEnvs env -- evaluate conditions of left and right 'branches'
evalCondition (Or c1 c2) allEnvs env =
	evalCondition c1 allEnvs env || evalCondition c2 allEnvs env -- evaluate conditions of left and right 'branches'

-- apply condition sequentially to execute filters before aggregate functions
applyConditions :: [Condition] -> [Binding] -> [Binding]
applyConditions cons initialEnvs =
	foldl (\envs cond -> filter (evalCondition cond envs) envs) initialEnvs cons

-- project only variables requested in SELECT clause
project :: [String] -> Binding -> Binding
project vars env = [(v, t) | v <- vars, Just t <- [lookupVar v env]]

-- convert 3-variable binding back to RDF Triple for output
bindingToTriple :: [String] -> Binding -> Maybe Triple
bindingToTriple [varS, varP, varO] env = do
	s <- lookupVar varS env
	p <- lookupVar varP env
	o <- lookupVar varO env
	return (s, p, o)
bindingToTriple _ _ = Nothing -- otherwise return nothing

-- takes parsed program and RDF graph and then filters contents of RDF graph against conditions in program 
executeQuery :: Query -> Dataset -> [Triple]
executeQuery (Select vars fromGraphs _ cons) ds =
	let
		-- creates bindings for triple, primary graph is implicit
		bindTriple :: String -> Bool -> Triple -> Binding
		bindTriple gName isPrimary (s,p,o) =
			let 
				explicitBinds = [ -- combines graph name with variable name when graph is explicitly specified
								(gName ++ ".?s",s)
								, (gName ++ ".?p",p)
								, (gName ++ ".?o",o)
								]
				implicitBinds = if isPrimary  -- only leaves bindings in first graph passed implicit
								then [("?s",s),("?p",p),("?o",o)] 
								else []
			in explicitBinds ++ implicitBinds -- return all bindings (explicit and implicit)

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
		triples = mapMaybe (bindingToTriple vars) projectedBindings
	in nub triples -- return result with removal of repeated triples
executeQuery (Union q1 q2) ds =
	let 
		results1 = executeQuery q1 ds
		results2 = executeQuery q2 ds
	in nub (results1 ++ results2) -- return concatenation of executed query on both branches of UNION
executeQuery (Inter q1 q2) ds =
	let
		results1 = executeQuery q1 ds
		results2 = executeQuery q2 ds
	in intersect results1 results2 -- return intersection of executed query on both branches of INTERSECTION
executeQuery (Group _ _) _ = []
executeQuery (Diff _ _) _ = []	

-- for testing, predefines generic content for RDF document, converts that String into list of Triple objects, predefines generic query in parsed format, runs executeQuery on predefined program and parsed triples, then outputs results as an unparsed string
--main :: IO ()
--main = 
--    do
		-- output task name then unparsed string of the result of calling executeQuery on specified query and provided dataset (graph reference, list of triples)
--        putStrLn "Task 1 results:"
--        putStr (unparseRDF (executeQuery query1 dataset1))
--        putStrLn "\nTask 2 results:"
--        putStr (unparseRDF (executeQuery query2 dataset2))
--        putStrLn "\nTask 3 results:"
--        putStr (unparseRDF (executeQuery query3 dataset3))
--		putStrLn "\nTask 4 results:"
--		putStr (unparseRDF (executeQuery query4 dataset4))
--		putStrLn "\nTask 5 results:"
--		putStr (unparseRDF (executeQuery query5 dataset5))
	
--    where 
        -- Task 1
		-- generic RDF graph
--	    rdfDataG11 = "<http://example.org/alice> <http://example.org/ont/name> \"Alice\" .\n<http://example.org/alice> <http://example.org/ont/worksFor> <http://example.org/uos> .\n<http://example.org/bob> <http://example.org/ont/name> \"Bob\" ."
--	    triplesG11 = parseRDF rdfDataG11
	    -- generic RDF graph
--		rdfDataG12 = "<http://example.org/charlie> <http://example.org/ont/name> \"Charlie\" .\n<http://example.org/charlie> <http://example.org/ont/studiesAt> <http://example.org/uos> .\n<http://example.org/dave> <http://example.org/ont/name> \"Dave\" ."
--	    triplesG12 = parseRDF rdfDataG12
--	    dataset1 = [("?g1",triplesG11),("?g2",triplesG12)]
		-- output the union of two graphs, leaving conditions on triples empty
--	    query1 = Union (Select ["?s","?p","?o"] ["?g1"] []) (Select ["?s","?p","?o"] ["?g2"] [])

	    -- Task 2
		-- generic RDF graph
--	    rdfData2 = "<http://example.org/alice> <http://example.org/ont/hasAge> 25 .\n<http://example.org/bob> <http://example.org/ont/hasAge> 19 .\n<http://example.org/charlie> <http://example.org/ont/hasAge> 30 .\n<http://example.org/dave> <http://example.org/ont/name> \"Dave\" ."
--	    triples2 = parseRDF rdfData2
--	    dataset2 = [("?g",triples2)]
		-- output all triples where predicate = <specified_uri> and object is <= specified_int
--	    query2 = Select ["?s","?p","?o"] ["?g"] [Eq (Var "?p" Nothing) (Const (URI "http://example.org/ont/hasAge")), Gte (Var "?o" Nothing) (Const (LitInt 21))]
	
	    -- Task 3
		-- generic RDF graph
--	    rdfData3 = "<http://example.org/alice> <http://example.org/ont/studiesAt> <http://example.org/uos> .\n<http://example.org/bob> <http://example.org/ont/worksFor> <http://example.org/uos> .\n<http://example.org/charlie> <http://example.org/ont/studiesAt> <http://example.org/oxford> .\n<http://example.org/dave> <http://example.org/ont/worksFor> <http://example.org/google> .\n<http://example.org/eve> <http://example.org/ont/name> \"Eve\" ."
--	    triples3 = parseRDF rdfData3
--	    dataset3 = [("?g",triples3)]
		-- output all triples where predicate is one of two URIs and object is a specified URI
--	    query3 = Select ["?s","?p","?o"] ["?g"] [And (Or (Eq (Var "?p" Nothing) (Const (URI "http://example.org/ont/studiesAt"))) (Eq (Var "?p" Nothing) (Const (URI "http://example.org/ont/worksFor")))) (Eq (Var "?o" Nothing) (Const (URI "http://example.org/uos")))]

		-- Task 4
		-- generic RDF graph
--		rdfData4 = "<http://example.org/alice> <http://example.org/ont/price> 100 .\n<http://example.org/alice> <http://example.org/ont/price> 200 .\n<http://example.org/bob> <http://example.org/ont/price> 50 .\n<http://example.org/bob> <http://example.org/ont/price> 150 .\n<http://example.org/charlie> <http://example.org/ont/name> \"Charlie\" ."
--		triples4 = parseRDF rdfData4
--		dataset4 = [("?quux",triples4)]
		-- output all triples where predicate is a specified URI, maximising the object
--		query4 = Select ["?s","?p","?o"] ["?quux"] [Eq (Var "?p" Nothing) (Const (URI "http://example.org/ont/price")), Max "?o" Nothing]

		-- Task 5
		-- generic RDF graph for xyzzy
--		rdfDataG51 = "<http://example.org/res1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/classA> .\n<http://example.org/res2> <http://example.org/ont/other> <http://example.org/classB> ."
--		triplesG51 = parseRDF rdfDataG51
		-- generic RDF graph for plugh
--		rdfDataG52 = "<http://example.org/classA> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/classC> ."
--		triplesG52 = parseRDF rdfDataG52
--		dataset5 = [("?xyzzy",triplesG51),("?plugh",triplesG52)]
		-- construct triples containing the subject and predicate of triples in g1 and the object of triples in g2 where: predicates in g1 are a specified URI, predicates in g2 are a specified URI, and the object in a triple in g1 matches the subject in a triple in g2
--		query5 = Select ["?s","?p","?plugh.?o"] ["?xyzzy","?plugh"] [And (Eq (Var "?p" Nothing) (Const (URI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))) (And (Eq (Var "?p" (Just "?plugh")) (Const (URI "http://www.w3.org/2000/01/rdf-schema#subClassOf"))) (Eq (Var "?o" Nothing) (Var "?s" (Just "?plugh"))))]