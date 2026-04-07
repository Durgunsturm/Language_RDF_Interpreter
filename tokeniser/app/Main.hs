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
    dataset <- loadDataset exprs env -- create dataset from environment
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
extractPath (URI _) = ""
extractPath (LitInt _) = ""

-- normalise RDF graphs in dataset
loadDataset :: [Expr] -> Env -> IO [(String,[Triple])]
loadDataset [] _ = return [] -- base case
loadDataset (Norm targetVar inVar outVar : xs) env = do
    case (lookup inVar env, lookup outVar env) of
        (Just inPath, Just outPath) -> do -- if input file path and output file path in environment
            normalise inPath outPath -- normalie input into output
            rdfData <- readFile outPath -- read normalised file contents
            let triples = parseRDF rdfData -- convert string into triples data type
            rest <- loadDataset xs env -- repeat on rest of Query
            return ((targetVar, triples) : rest) -- return list of target variable and associated triples as dataset for executeQuery
        _ -> error ("Undefined variable in Norm command: missing assignment for " ++ inVar ++ " or " ++ outVar)
loadDataset (_ : xs) env = loadDataset xs env -- for all elements of Query that aren't (Norm target in out)

processQueries :: [Expr] -> Env -> Dataset -> IO ()
processQueries [] _ _ = return () -- base case
processQueries (Queries q : xs) env dataset = do
    -- for testing
    putStrLn "Dataset contents:"
    print dataset
    putStrLn "Queries:"
    print q
    -- main code
    let
        resultTriples = executeQuery q dataset -- execute query against dataset
        resultStr = unparseRDF resultTriples -- unparse query result into string using unparseRDF
        (fromVars, toVars) = getFromTo q -- get from file path and to file path
        isConsoleOutput = case toVars of
            Just toVar -> False
            Nothing -> True
    if isConsoleOutput then do -- write to console
        putStrLn "Query Result:"
        putStrLn resultStr
    else do -- write to to file
        let outVar = case toVars of
                Just vars -> head vars -- filepath
                Nothing -> "" -- shouldn't ever occur as Nothing toVars already handled in isConsoleOutput
        case lookup outVar env of
            Just outPath -> do -- output path exists, write to output path
                writeFile outPath resultStr
                putStrLn ("Query Result written to: " ++ outPath)
            Nothing -> do -- else write to console and tell user failed to write to file
                putStrLn ("Failed to write to: " ++ outVar ++ "Printing to console:")
                putStrLn resultStr
    processQueries xs env dataset
processQueries (_:xs) env dataset = processQueries xs env dataset -- if expression isn't a query, recursively call on list of expressions

-- get from file path and to file path, only Select statements contain file paths
getFromTo :: Query -> ([String], Maybe [String])
getFromTo (Select _ from (Just to) _) = (from, Just to)
getFromTo (Select _ from Nothing _) = (from, Nothing)
getFromTo (Union q1 _) = getFromTo q1
getFromTo (Group q1 _) = getFromTo q1
getFromTo (Inter q1 _) = getFromTo q1
getFromTo (Diff q1 _) = getFromTo q1