module Main (
  main
) where 
import Tokens
import Grammar
import Normalise
import Query
import System.Environment
import Control.Exception
import System.IO
import Data.List (sort)

type Env = [(String, String)]

main :: IO ()
main = catch main' noParse

main' = do
    args <- getArgs
    case args of 
        (fileName : _ ) -> do
            sourceText <- readFile fileName
            -- putStrLn ("Parsing : " ++ sourceText ++ "\n")
            let tokens = alexScanTokens sourceText
            -- putStrLn ("Lexed as " ++ show tokens ++ "\n")
            let parsedProg = parseCalc tokens
            -- putStrLn ("Parsed as " ++ show parsedProg)
            runProgram parsedProg -- run program
        [] -> do
            putStrLn "Error: No input file provided."

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

runProgram :: [Expr] -> IO()
runProgram exprs = do
    let env = buildEnv exprs -- from expressions, link variable name to file path
    dataset <- loadDataset exprs env -- create dataset from environment
    processQueries exprs env dataset -- process queries

buildEnv :: [Expr] -> Env
buildEnv [] = [] -- base case
buildEnv (Variable varName rdfTerm : xs) = -- graph variables extracted from expression list and path to file linked to variable name in environment
    let path = extractPath rdfTerm -- will only get valid file path if rdfTerm is LitStr
    in (varName, path) : buildEnv xs -- recursively build list of graph variable names and associated file path
buildEnv (_:xs) = buildEnv xs -- skip expressions that aren't variables

-- convert file path from RDFTerm into string, only LitStr RDFTerms produce usable file paths
extractPath :: RDFTerm -> String
extractPath (LitStr s) = s
extractPath (URI _) = ""
extractPath (LitInt _) = ""

-- normalise RDF graphs in dataset
loadDataset :: [Expr] -> Env -> IO [(String,[Triple])]
loadDataset [] _ = return [] -- base case
loadDataset (Norm targetVar inVar normVar : xs) env = do
    case (lookup inVar env, lookup normVar env) of
        (Just inPath, Just normPath) -> do -- if input file path and normalise file path in environment
            normalise inPath normPath -- normalise input into normalised file
            rdfData <- readFile normPath -- read normalised file contents
            let triples = parseRDF rdfData -- convert rdf graph string into triples data type
            rest <- loadDataset xs env -- repeat on rest of Query
            return ((targetVar, triples) : rest) -- return list of target variable and associated triples as dataset for executeQuery
        _ -> error "Variable in 'init' not assigned"
loadDataset (_ : xs) env = loadDataset xs env -- for all elements of Query that aren't of form (Norm target input normalise)

processQueries :: [Expr] -> Env -> Dataset -> IO ()
processQueries [] _ _ = return () -- base case
processQueries (Queries q : xs) env dataset = do -- Expressions matching Query pattern
    -- for testing
    -- putStrLn "Dataset contents:"
    -- print dataset
    -- putStrLn "Queries:"
    -- print q
    -- main code
    let
        resultTriples = executeQuery q dataset -- execute query against dataset
        resultStr = unparse resultTriples -- sort triples and unparse into single string
        (fromVars, toVars) = getFromTo q -- get from file path and to file path
        isConsoleOutput = case toVars of
            Just toVar -> False -- case when TO clause not included in program
            Nothing -> True -- case when TO clause included in program
    if isConsoleOutput then do -- write to console
        -- putStrLn "Query Result:"
        putStrLn resultStr
    else do -- write to file
        let outVar = case toVars of -- extract head of toVars into outVar (removing this and instead using 'outVar = head toVars' causes a syntax error)
                Just vars -> head vars -- filepath
                Nothing -> "" -- shouldn't ever occur as Nothing toVars already handled in isConsoleOutput
        case lookup outVar env of
            Just outPath -> do -- output path exists, write to output path
                writeFile outPath resultStr
                putStrLn ("Query Result written to: " ++ outPath) -- output to console informing user which file has been written to
            Nothing -> do -- else write to console and tell user failed to write to file
                putStrLn ("Failed to write to: " ++ outVar ++ "Printing to console:")
                putStrLn resultStr -- output result to console
    processQueries xs env dataset -- recursively call on rest of expressions list
processQueries (_:xs) env dataset = processQueries xs env dataset -- if expression isn't a query, recursively call on list of expressions

-- get from file path and to file path, only Select statements contain file paths
getFromTo :: Query -> ([String], Maybe [String])
getFromTo (Select _ from (Just to) _) = (from, Just to) -- TO clause exists in program
getFromTo (Select _ from Nothing _) = (from, Nothing) -- TO clause doesn't exist in program
-- TO clause is only defined in SELECT statements
getFromTo (Union q1 _) = getFromTo q1
getFromTo (Group q1 _) = getFromTo q1
getFromTo (Inter q1 _) = getFromTo q1
getFromTo (Diff q1 _) = getFromTo q1

unparseTerm :: RDFTerm -> String
unparseTerm (URI u) = "<" ++ u ++ ">"
unparseTerm (LitStr s) = "\"" ++ s ++ "\""
unparseTerm (LitInt i) = show i

unparseTriple :: Triple -> String
unparseTriple (s, p, o) = unparseTerm s ++ " " ++ unparseTerm p ++ " " ++ unparseTerm o ++ " ."

unparse :: [Triple] -> String
unparse = unlines . map unparseTriple . sort