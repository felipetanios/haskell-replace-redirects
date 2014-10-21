module Main where

import Text.Regex.PCRE ((=~))
import Data.List (intercalate)
import Network.HTTP

type ListOfMatches = [[String]]

main :: IO ()
main = interact transformBody

transformBody :: String -> String
transformBody = (intercalate "\n") . transformLines . lines

transformLines :: [String] -> [String]
transformLines [] = []
transformLines (line:xs) = transformed:(transformLines xs)
  where
    transformed = transformLine line (matchAgainstUrlRegex line)

-- Given a line *without* a URL, return the line unchanged.
-- Given a line *with* a URL, return just the URL (for now)
-- It knows if it has a URL because of the second argument. If it's empty,
-- there's no URL.
transformLine :: String -> ListOfMatches -> String
transformLine original [] = original
transformLine original (x:xs) = url
  where
    fullLine = x !! 0
    everythingButUrl = x !! 1
    url = x !! 2

-- When it matches:
-- matchAgainstUrlRegex "hello: http://g.co"
-- [["hello: http://g.co","hello: ","http://g.co"]]
--
-- If it doesn't match, it returns an empty list.
matchAgainstUrlRegex :: String -> ListOfMatches
matchAgainstUrlRegex = (=~ "(.+)(http://.*)$")


-- Playing with Network.HTTP
-- response <- simpleHTTP (getRequest "http://rdd.me")
-- header = fmap (retrieveHeaders HdrLocation) response -- Array of 1 Header
-- fmap (hdrValue . head) header -- Right "http://www.readability.com/shorten"
