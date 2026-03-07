module Normalise (
    normalise
) where

--Translates turtle abbrieviations into standard form.
--File is required to be normalised fully before querying.


--Translates an abbreviated ttl file into a normalised ttl file
normalise :: FilePath -> FilePath -> IO ()
normalise input output = do
  contents <- readFile input
  writeFile output contents

--Passes test files automatically to normalise
autoNormal :: IO ()
autoNormal = normalise "normTest/normInput.ttl" "normTest/normOutput.ttl"