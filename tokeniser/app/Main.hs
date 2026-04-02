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

type Env = [(String, String)]

main :: IO ()
main = catch main' noParse

main' = do
    args <- getArgs
    case args of 
        (fileName : _ ) -> do
            sourceText <- readFile fileName
            putStrLn ("Parsing : " ++ sourceText ++ "\n")
            let tokens = alexScanTokens sourceText
            -- putStrLn ("Lexed as " ++ show tokens ++ "\n")
            let parsedProg = parseCalc tokens
            putStrLn ("Parsed as " ++ show parsedProg)
            runProgram parsedProg -- run program
        [] -> do
            putStrLn "Error: No input file provided."

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

runProgram :: [Expr] -> IO()
runProgram exprs = do
    let env = buildEnv exprs -- build environment from expressions
    dataset <- loadDataset env -- create dataset from environment
    processQueries exprs env dataset -- process queries

buildEnv :: [Expr] -> Env
buildEnv [] = [] -- base case
buildEnv (Variable varName rdfTerm : xs) = -- graph variables extracted from expression list and path to file linked to variable name in environment
    let path = extractPath rdfTerm
    in (varName, path) : buildEnv xs
buildEnv (_:xs) = buildEnv xs -- skip expressions that aren't variables

-- convert file path from RDFTerm into string which can be read
extractPath :: RDFTerm -> String
extractPath (LitStr s) = s
extractPath (URI s) = s
extractPath (LitInt _) = ""

-- normalise RDF graphs in dataset
loadDataset :: Env -> IO [(String,[a])]
loadDataset [] = return [] -- base case
loadDataset ((var, path):xs) = do
    let normPath = "norm" ++ path -- create normalised file path
    normalise path normPath -- normalise contents of path into normPath
    rdfData <- readFile normPath -- read contents of normPath into rdfData
    let triples = parseRDF rdfData -- parse normalised RDF graph into required data type
    rest <- loadDataset xs -- recursively act on rest of list
    return ((var, triples) : rest) -- return list of tuples containing a variable name and associated list of triples

processQueries :: [Expr] -> Env -> [(String, b)] -> IO ()
processQueries [] _ _ = return () -- base case
processQueries (Queries q : xs) env dataset = do
    let
        resultTriples = executeQuery q dataset -- execute query against dataset
        resultStr = unparseRDF resultTriples -- unparse query result into string using unparseRDF
        (fromVars, toVars) = getFromTo q -- get from file path and to file path
        isConsoleOutput = null toVars -- check whether to write to console or to file
    if isConsoleOutput then do -- write to console
        putStrLn "Query Result:"
        puStrLn resultStr
    else do -- write to to file
        let outVar = head toVars -- first file path in toVars
        case lookup outVar env of
            Just outPath -> do -- output path exists, write to output path
                writeFile outPath resultStr
                putStrLn $ "Query Result written to: " ++ outPath
            Nothing -> do -- else write to console and tell user failed to write to file
                putStrLn $ "Failed to write to: " ++ outVar ++ "Printing to console:"
                putStrLn resultStr
    processQueries xs env dataset
processQueries (_:xs) env dataset = processQueries xs env dataset -- if expression isn't a query, recursively call on list of expressions

-- get from file path and to file path, only Select statements contain file paths
getFromTo :: Query -> ([String], [String])
getFromTo (Select _ from to _) = (from, to)
getFromTo (Union q1 _) = getFromTo q1
getFromTo (Group q1 _) = getFromTo q1
getFromTo (Inter q1 _) = getFromTo q1
getFromTo (Diff q1 _) = getFromTo q1
