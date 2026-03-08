module Normalise (
    normalise
) where
import Text.Parsec hiding (Empty)
import Text.Parsec.String (Parser)
import Control.Applicative (some)

--Translates turtle abbrieviations into standard form.
--File is required to be normalised fully before querying.

-- Types --

--Whether the object is the section of the base or the whole object
data Obj = Sect String | Single String deriving Show

newtype Base = B String deriving Show --These three types exist to occasionally enforce specific line types as parameters
data Prefix = P String String deriving Show
data Triple = T String String String Obj deriving Show
data TTLine = Ba Base | Pre Prefix | Comp [Triple] | Whole String deriving Show
--Base has base URI for the file (1st line)
--Prefix has prefix onto the base with its whole URI (2nd line)
--Triple has subject, prefix, predicate and object (3+ line)
--Comp has multiple objects with same subject and predicate
--Whole is an unsimplified line (ie the expected output)

-- Unparsing --

--Creates a subject part of a triple
assembleSubj :: Triple -> Base -> String
assembleSubj (T sub pre pred obj) (B base) = '<' : base ++ sub ++ ">"

--Creates a predicate part of a triple
assemblePred :: Triple -> [Prefix] -> String
assemblePred _ [] = "No predicates found"
assemblePred (T sub pre pred obj) (P name uri:prefixes)
  | pre == name = '<' : uri ++ pred ++ ">"
  | otherwise = assemblePred (T sub pre pred obj) prefixes

--Creates an object part of a triple
assembleObj :: Triple -> Base -> String
assembleObj (T sub pre pred (Single obj)) (B base) = obj
assembleObj (T sub pre pred (Sect obj)) (B base) = '<' : base ++ obj ++ ">"

--Unparses a Triple type to a list of normalised lines
unParseTriple :: Triple -> Base -> [Prefix] -> String
unParseTriple tri base prefixes =
    assembleSubj tri base ++ " " ++
    assemblePred tri prefixes ++ " " ++
    assembleObj tri base ++ " ."

--Unparses a list of Triple types to a list of normalised lines
unParseTriples :: [Triple] -> Base -> [Prefix] -> [String]
unParseTriples [] _ _ = []
unParseTriples (t:ts) base prefixes = unParseTriple t base prefixes : unParseTriples ts base prefixes

-- Parsing --

--Reads all chars until it hits the excluded char
parsePart :: Char -> Parser String
parsePart ex = some (noneOf [ex]) 

parseBracket :: Bool -> Parser String
parseBracket keepBracket = do
  char '<'
  content <- parsePart '>'
  char '>'
  if keepBracket
    then return ('<' : content ++ ">")
    else return content

parseSpeech ::  Parser String
parseSpeech = do
  char '"'
  content <- parsePart '"'
  char '"'
  return ('"' : content ++ "\"") --Always reattaches speech marks

--Parses a space separated section of that could have angle brackets or nothing
parseSection :: Bool -> Parser String
parseSection keepBracket = do 
  spaces --Consumes any spaces before and after section
  val <- try (parseBracket keepBracket) <|> try parseSpeech <|> try (parsePart ' ')
  spaces
  return val


parseBase :: Parser TTLine
parseBase = do
  string "base"
  uri <- parseSection False -- Reads actual base identifier
  char '.'
  return $ Ba (B uri)

parsePrefix :: Parser TTLine
parsePrefix = do
  string "prefix"
  name <- parseSection True -- Reads name of prefix, which must have no spaces
  char ':'
  uri <- parseSection False
  char '.'
  return $ Pre (P name uri)

--Consumes @ and then checks if its a base or prefix
parseBaseOrPrefix :: Parser TTLine
parseBaseOrPrefix = do
  string "@"
  try parseBase <|> parsePrefix


--Parses objects separated by ',' and returns each object value
parseSamePred :: Parser String
parseSamePred = do
  char ','
  parseSection False

--Recurses back to parseTripleSect to handle multiple triples with same subject
parseDiffTriple :: String -> Parser [Triple]
parseDiffTriple subj = do
  spaces >> char ';' >> spaces
  parseTripleSect subj

--Compiles subject, prefix, predicate and object array to array of triples
placeIntoTriples :: String -> String -> String -> [String] -> [Triple]
placeIntoTriples subj pre pred = map (T subj pre pred . Sect)

--Parses objects separated by ';'
parseSameName :: String -> String -> String -> String -> Parser [Triple]
parseSameName subj pre pred firstObj = do
  objs <- many parseSamePred --Optional comma separated object strings
  let trips = placeIntoTriples subj pre pred (firstObj : objs) --Array of objects with same subject and predicate
  nextObjs <- try parseEndLines <|> try (parseDiffTriple subj) --Combines objects after ;
  return $ trips ++ nextObjs

--Handles end of triple block
parseEndLines :: Parser [Triple]
parseEndLines = do
  spaces
  char '.'
  return []


--Loads a single object into TTLine
parseEndLine :: Triple -> Parser [Triple]
parseEndLine trip = do
  char '.'
  return [trip]

--Parses different ways of writing the object
parseTripleSect :: String -> Parser [Triple]
parseTripleSect subj = do
  prefix <- parsePart ':'
  char ':'
  pred <- parseSection True
  obj <- parseSection False
  try (parseEndLine (T subj prefix pred (Sect obj))) <|> try (parseSameName subj prefix pred obj)

--Converts triple array into line data type
handleTripleSect :: String -> Parser TTLine
handleTripleSect subj = do
  trips <- parseTripleSect subj
  return $ Comp trips


--Checks if its a whole inline triple rather than a triple block
parseWholeTriple :: String -> Parser TTLine
parseWholeTriple subj = do
  char '<' --Required to distinguish from triple blocks
  pred <- parsePart '>'
  char '>'
  obj <- parseSection True
  char '.'
  return $ Whole ('<' : subj ++ "> <" ++ pred ++ "> " ++ obj ++ " .") --Reproduces the read triple to be output as it is

--Consumes the first section and then checks what kind of triple it is
parseTriple :: Parser TTLine
parseTriple = do
  first <- parseSection False
  try (parseWholeTriple first) <|> try (handleTripleSect first)

--Parses a single input line into each type of line it could be
parseLine :: Parser TTLine
parseLine = do
  try parseBaseOrPrefix <|> try parseTriple

--Hands one line into the parser
handleLine :: String -> TTLine
handleLine xs = case parse (parseLine <* eof) "" xs of
  Left _ -> error "Parsing failed"
  Right x -> x

-- Init --

unParseAllLines :: [TTLine] -> Base -> [Prefix] -> [String]
unParseAllLines [] _ _ = []
unParseAllLines (Whole line:lines) base prefixes = line : unParseAllLines lines base prefixes --Add existing line to output
unParseAllLines (Ba base:lines) _ _ = unParseAllLines lines base [] --New base block
unParseAllLines (Pre prefix:lines) base prefixes = unParseAllLines lines base (prefix:prefixes) --New prefix block
unParseAllLines (Comp triples:lines) base prefixes = unParseTriples triples base prefixes ++ unParseAllLines lines base prefixes --Unparse a line to a list of triples

--Translates an abbreviated ttl file into a normalised ttl file
normalise :: FilePath -> FilePath -> IO ()
normalise input output = do
  contents <- readFile input
  lines <- return $ lines contents
  let parsedLines = map handleLine lines
  let outputLines = unParseAllLines parsedLines (B "") []
  writeFile output $ unlines outputLines

--Passes test files automatically to normalise
autoNormal :: IO ()
autoNormal = normalise "normTest/normInput.ttl" "normTest/normOutput.ttl"


-- Testing --

--Parsing tests

testSection :: String -> Bool -> String
testSection xs keepBracket = case parse (parseSection keepBracket <* eof) "" xs of
  Left _ -> error "Parsing failed"
  Right x -> x

testBase :: String -> TTLine
testBase xs = case parse (parseBase <* eof) "" (tail xs) of
  Left _ -> error "Parsing failed"
  Right x -> x

testPrefix :: String -> TTLine
testPrefix xs = case parse (parsePrefix <* eof) "" (tail xs) of
  Left _ -> error "Parsing failed"
  Right x -> x

testPreBase :: String -> TTLine
testPreBase xs = case parse (parseBaseOrPrefix <* eof) "" xs of
  Left _ -> error "Parsing failed"
  Right x -> x

testTriple :: String -> TTLine
testTriple xs = case parse (parseTriple <* eof) "" xs of
  Left _ -> error "Parsing failed"
  Right x -> x