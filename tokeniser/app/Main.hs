module Main (
  main
) where 
import Tokens
import Grammar
import System.Environment
import Control.Exception
import System.IO


main :: IO ()
main = catch main' noParse

main' = do
    args <- getArgs
    case args of 
        (fileName : _ ) -> do
            sourceText <- readFile fileName
            putStrLn ("Parsing : " ++ sourceText)
            let tokens = (alexScanTokens sourceText)
            putStrLn ("Lexed as " ++ show tokens)
            let parsedProg = parseCalc tokens
            putStrLn ("Parsed as " ++ show parsedProg)
        [] -> do
            putStrLn "Error: No input file provided."

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

