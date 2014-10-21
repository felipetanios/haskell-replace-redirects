module Main where

import Text.Regex.PCRE ((=~))
import Data.List (intercalate)
import Network.HTTP
import Network.Stream (ConnError)

type ListOfMatches = [[String]]

main :: IO ()
main = interact transformBody

transformBody :: String -> String
transformBody = (intercalate "\n") . transformLines . lines

transformLines :: [String] -> [String]
transformLines = map transformLine

-- Given a line *without* a URL, return the line unchanged.
-- Given a line *with* a URL, return just the URL (for now)
-- It knows if it has a URL because of the second argument. If it's empty,
-- there's no URL.
transformLine :: String -> String
transformLine original
  | matchAgainstUrlRegex original == [] = original
  | otherwise = url
    where
      fullLine = match !! 0
      everythingButUrl = match !! 1
      url = match !! 2
      match = head $ matchAgainstUrlRegex original

-- When it matches:
-- matchAgainstUrlRegex "hello: http://g.co"
-- [["hello: http://g.co","hello: ","http://g.co"]]
--
-- If it doesn't match, it returns an empty list.
matchAgainstUrlRegex :: String -> ListOfMatches
matchAgainstUrlRegex = (=~ "(.+)(http://.*)$")

findUnshortenedUrl :: String -> IO (Either Network.Stream.ConnError String)
findUnshortenedUrl shortenedUrl = do
  response <- simpleHTTP (getRequest shortenedUrl)
  let header = fmap (retrieveHeaders HdrLocation) response
  let headerValue = fmap (hdrValue . head) header
  return headerValue
