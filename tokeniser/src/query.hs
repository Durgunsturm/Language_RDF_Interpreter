--Queries normalised turtle files
module Query (
        parseRDF,
        executeQuery,
        Dataset,
        Triple,
        Binding
) where
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Data.List
import Grammar

type Dataset = [(String, [Triple])] -- List of graph names and associated triples (parsed from input graphs)
type Triple = (RDFTerm, RDFTerm, RDFTerm) -- parsed version of the RDF graphs stored in a datatype that allows for easy manipulation in condition matching
type Binding = [(String, RDFTerm)] -- list of variable name ("?s", "?p", "?o", "?graph1.?s", etc...) and associated RDFTerm

instance Eq RDFTerm where -- define equality for RDFTerm datatype
        (URI u1) == (URI u2) = u1 == u2
        (LitInt i1) == (LitInt i2) = i1 == i2
        (LitStr s1) == (LitStr s2) = s1 == s2
        _ == _ = False

instance Ord RDFTerm where
    compare (LitStr s1) (LitStr s2) = compare s1 s2
    compare (LitStr _) _ = LT
    compare _ (LitStr _) = GT
    compare (LitInt i1) (LitInt i2) = compare i1 i2
    compare (LitInt _) _ = LT
    compare _ (LitInt _) = GT
    compare (URI u1) (URI u2) = compare u1 u2

-- separates line into 3 strings: subject, predicate, object
tokenise :: String -> [String]
tokenise [] = [] -- base case
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
tokenise xs = -- otherwise
        let (token, rest) = break (\c -> c == ' ' || c == '\t') xs
        in token : tokenise rest

-- converts tokenised strings into RDFTerm values
parseTerm :: String -> RDFTerm
parseTerm ('<':xs) | not (null xs) && last xs == '>' = URI (init xs) -- when x == '<', xs not null and final xs character is '>', produce a URI RDF term
parseTerm ('"':xs) | not (null xs) && last xs == '"' = LitStr (init xs) -- when x == '"', xs not null and final xs character is '"', produce a String RDF term
parseTerm str -- otherwise
        | not (null str) && all isDigit (if head str == '-' then tail str else str) = LitInt (read str) -- if characters in term are integers, produce an Integer RDF term
        | otherwise = LitStr str -- default to string when all other options fail

-- tokenises individual line and then converts elements into RDFTerms, returns a Maybe Triple to ensure a value is always returned
parseLine :: String -> Maybe Triple
parseLine line =
        case filter (/= ".") (tokenise line) of -- filter out '.' characters
                [s, p, o] -> Just (parseTerm s, parseTerm p, parseTerm o) -- Maybe Triple containing parsed terms for subject, predicate,and object
                _ -> Nothing -- otherwise

-- parses each line individually, using Haskell 'lines' function to separate on '\n' characters in doc
parseRDF :: String -> [Triple]
parseRDF doc = mapMaybe parseLine (lines doc) -- parse lines of RDF graph into [Triple]

-- resolve variable name based on explicity/implicit graph scopes
resolveVar :: String -> GraphRef -> String
resolveVar var Nothing = var
resolveVar var (Just g) = g ++ "." ++ var

-- resolve operand to maybe RDFTerm
evalOperand :: Operand -> Binding -> Maybe RDFTerm
evalOperand (Const term) _ = Just term
evalOperand (Var var gRef) env = lookup (resolveVar var gRef) env

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
                isTargetVar k = k == var || dropWhile (/= '.') k == "." ++ var  -- target variable boolean
                groupingKey = filter (\(k, _) -> not (isTargetVar k)) env -- filter triples in binding to only match against target variable
                matchesGroup key targetEnv = all (\(k, v) -> lookup k targetEnv == Just v) key
                group = [e | e <- allEnvs, matchesGroup groupingKey e]
                intVals = [i | e <- group, Just (LitInt i) <- [lookup resolvedVar e]] -- check to ensure variable being maximised is an integer
        in case lookup resolvedVar env of
                Just (LitInt val) -> not (null intVals) && val == maximum intVals -- maximum can fetch more than one valid triple, assuming those triples don't match on at least one other aspect (e.g. maximise object, if subject in two max triples is different output both triples)
                _ -> False -- otherwise condition fails
evalCondition (Min var gRef) allEnvs env = -- same logic as maximise, just replace Just val condition in return with minimum instead of maximum
        let
                resolvedVar = resolveVar var gRef
                isTargetVar k = k == var || dropWhile (/= '.') k == "." ++ var
                groupingKey = filter (\(k, _) -> not (isTargetVar k)) env
                matchesGroup key targetEnv = all (\(k, v) -> lookup k targetEnv == Just v) key
                group = [e | e <- allEnvs, matchesGroup groupingKey e]
                intVals = [i | e <- group, Just (LitInt i) <- [lookup resolvedVar e]]
        in case lookup resolvedVar env of
                Just (LitInt val) -> not (null intVals) && val == minimum intVals -- minimum can fetch more than one valid triple, assuming those triples don't match on at least one other aspect (e.g. minimise object, if subject in two min triples is different output both triples)
                _ -> False -- otherwise condition fails
evalCondition (Not c) allEnvs env =
        not (evalCondition c allEnvs env) -- negate result of evaluated conditions inside NOT operation
evalCondition (And c1 c2) allEnvs env =
        evalCondition c1 allEnvs env && evalCondition c2 allEnvs env -- evaluate conditions of left and right 'branches', performing AND on result
evalCondition (Or c1 c2) allEnvs env =
        evalCondition c1 allEnvs env || evalCondition c2 allEnvs env -- evaluate conditions of left and right 'branches', performing OR on result
evalCondition _ _ _ = False -- default to False, should never be matched against

-- apply condition sequentially to execute filters
applyConditions :: [Condition] -> [Binding] -> [Binding]
applyConditions cons initialEnvs = foldl applySingle initialEnvs cons
        where applySingle envs cond = 
			case cond of -- Avg, Sum and Count create new triples so have to be handled sepparate to filter-type conditions
				Avg var gRef -> computeAverage var gRef envs
				Sum var gRef -> computeSum var gRef envs
				Count var gRef -> computeCount var gRef envs
				_ -> filter (evalCondition cond envs) envs -- all other conditions (filter-type conditions)

computeAverage :: String -> GraphRef -> [Binding] -> [Binding]
computeAverage var gRef envs =
        let
                resolvedVar = resolveVar var gRef -- same resolved variable + grouping logic as in Max and Min
                isTargetVar k = k == var || dropWhile (/= '.') k == "." ++ var
                getGroupKey env = filter (\(k,_) -> not (isTargetVar k)) env

                -- produces list of lists, each containing group of triples which individually need to be averaged
                grouped = groupBy (\e1 e2 -> getGroupKey e1 == getGroupKey e2) $ sortBy (\e1 e2 -> compare (getGroupKey e1) (getGroupKey e2)) envs

                -- takes list of bindings (group) and calculates average across that group, return a single triple
                processGroup :: [Binding] -> [Binding]
                processGroup [] = [] -- base case
                processGroup group = -- non empty group
                        let
                                firstEnv = head group
                                groupKey = getGroupKey firstEnv
                                intVals = [i | e <- group, Just (LitInt i) <- [lookup resolvedVar e]] -- check to ensure variable being average is an integer
                        in ([[(resolvedVar, LitInt (sum intVals `div` length intVals))] ++ groupKey | not (null intVals)])
        in concatMap processGroup grouped -- map process group across all groups in grouped and concatenate result

computeSum :: String -> GraphRef -> [Binding] -> [Binding]
computeSum var gRef envs =
        let -- same logic as computeAverage, bar division in processGroup else clause
                resolvedVar = resolveVar var gRef
                isTargetVar k = k == var || dropWhile (/= '.') k == "." ++ var
                getGroupKey env = filter (\(k,_) -> not (isTargetVar k)) env

                grouped = groupBy (\e1 e2 -> getGroupKey e1 == getGroupKey e2) $ sortBy (\e1 e2 -> compare (getGroupKey e1) (getGroupKey e2)) envs

                processGroup :: [Binding] -> [Binding]
                processGroup [] = []
                processGroup group =
                        let
                                firstEnv = head group
                                groupKey = getGroupKey firstEnv
                                intVals = [i | e <- group, Just (LitInt i) <- [lookup resolvedVar e]]
                        in ([[(resolvedVar, LitInt (sum intVals))] ++ groupKey | not (null intVals)]) -- no need to divide sum by length of intVals as average isn't being calculated
        in concatMap processGroup grouped

computeCount :: String -> GraphRef -> [Binding] -> [Binding]
computeCount var gRef envs =
	let -- same logic as computeAverage, computeSum, just return size of group in processGroup instead
                resolvedVar = resolveVar var gRef
                isTargetVar k = k == var || dropWhile (/= '.') k == "." ++ var
                getGroupKey env = filter (\(k,_) -> not (isTargetVar k)) env

                grouped = groupBy (\e1 e2 -> getGroupKey e1 == getGroupKey e2) $ sortBy (\e1 e2 -> compare (getGroupKey e1) (getGroupKey e2)) envs

                processGroup :: [Binding] -> [Binding]
                processGroup [] = []
                processGroup group =
                        let
                                firstEnv = head group
                                groupKey = getGroupKey firstEnv
                        in ([[(resolvedVar, LitInt (length group))] ++ groupKey]) -- count only returns the size of the group
        in concatMap processGroup grouped 

-- project only variables requested in SELECT clause
project :: [String] -> Binding -> Binding
project vars env = [(v, t) | v <- vars, Just t <- [lookup v env]]

-- convert 3-variable binding back to RDF Triple for output
bindingToTriple :: [String] -> Binding -> Maybe Triple
bindingToTriple [varS, varP, varO] env = do
        s <- lookup varS env
        p <- lookup varP env
        o <- lookup varO env
        return (s, p, o)
bindingToTriple _ _ = Nothing -- otherwise return nothing

-- takes parsed program and RDF graph and then filters contents of RDF graph against conditions defined in program 
executeQuery :: Query -> Dataset -> [Triple]
executeQuery (Select vars fromGraphs _ Nothing) ds = -- case where WHERE clause doesn't exist
        let
                -- creates bindings for triple, primary graph is implicit
                bindTriple :: String -> Bool -> Triple -> Binding
                bindTriple gName isPrimary (s,p,o) =
                        let
                                -- combines graph name with variable name when graph is explicitly specified
                                explicitBinds = [(gName ++ ".?s",s), (gName ++ ".?p",p), (gName ++ ".?o",o)]
                                implicitBinds = if isPrimary  -- only leaves bindings in first graph passed implicit
                                                                then [("?s",s),("?p",p),("?o",o)]
                                                                else []
                        in explicitBinds ++ implicitBinds -- return all bindings (explicit and implicit)

                -- extract specified graphs from dataset
                graphsData = [(g,ts) | g <- fromGraphs, Just ts <- [lookup g ds]]

                -- build list of bindings for each graph
                buildGraphBindings :: [(String, [Triple])] -> [[Binding]]
                buildGraphBindings [] = [] -- base case
                buildGraphBindings ((g1, ts1):rest) = -- concatenate bound triples of primary graph to bound triples of the rest of the graphs
                        map (bindTriple g1 True) ts1 : map (\(g,ts) -> map (bindTriple g False) ts) rest

                -- cartesian product of bindings across all queried graphs
                combinations = sequence (buildGraphBindings graphsData)

                initialBindings = map concat combinations

                -- evaluate and project
                filteredBindings = applyConditions [] initialBindings
                projectedBindings = map (project vars) filteredBindings
                triples = mapMaybe (bindingToTriple vars) projectedBindings
        in nub triples -- return result with removal of repeated triples
executeQuery (Select vars fromGraphs _ (Just cons)) ds = -- case where WHERE clause does exist
        let
                -- creates bindings for triple, primary graph is implicit
                bindTriple :: String -> Bool -> Triple -> Binding
                bindTriple gName isPrimary (s,p,o) =
                        let
                                -- combines graph name with variable name when graph is explicitly specified
                                explicitBinds = [(gName ++ ".?s",s), (gName ++ ".?p",p), (gName ++ ".?o",o)]
                                implicitBinds = if isPrimary  -- only leaves bindings in first graph passed implicit
                                                                then [("?s",s),("?p",p),("?o",o)]
                                                                else []
                        in explicitBinds ++ implicitBinds -- return all bindings (explicit and implicit)

                -- extract specified graphs from dataset
                graphsData = [(g,ts) | g <- fromGraphs, Just ts <- [lookup g ds]]

                -- build list of bindings for each graph
                buildGraphBindings :: [(String, [Triple])] -> [[Binding]]
                buildGraphBindings [] = [] -- base case
                buildGraphBindings ((g1, ts1):rest) = -- concatenate bound triples of primary graph to bound triples of the rest of the graphs
                        map (bindTriple g1 True) ts1 : map (\(g,ts) -> map (bindTriple g False) ts) rest

                -- cartesian product of bindings across all queried graphs
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
        in nub (results1 ++ results2) -- return union of executed query on both branches of UNION
executeQuery (Inter q1 q2) ds =
        let
                results1 = executeQuery q1 ds
                results2 = executeQuery q2 ds
        in intersect results1 results2 -- return intersection of executed query on both branches of INTERSECTION
executeQuery (Group _ _) _ = []
executeQuery (Diff q1 q2) ds =
        let
                results1 = executeQuery q1 ds
                results2 = executeQuery q2 ds
        in results1 \\ results2 -- return difference of executed query on both branches of DIFFERENCE