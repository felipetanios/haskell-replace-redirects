module Main where

import Text.Regex.PCRE ((=~))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Network.HTTP (simpleHTTP, getRequest, findHeader, HeaderName(HdrLocation))

type ListOfMatches = [[String]]

main :: IO ()
-- `a >>= b` takes the result of a and passes it to b, ~monadically~
-- `=<<` does the same thing, but in reverse order.
-- So `=<<` flows right to left, and `>>=` flows left to right
main = putStr =<< transformBody =<< getContents

transformBody :: String -> IO String
transformBody = (fmap unlines) . transformLines . lines

transformLines :: [String] -> IO [String]
transformLines = mapM transformLine

-- Given a line *without* a URL, return the line unchanged.
-- Given a line *with* a URL, change the possibly-shortened URL to its longer
-- version.
transformLine :: String -> IO String
transformLine original = transformMatch original $ matchAgainstUrlRegex original

transformMatch :: String -> ListOfMatches -> IO String
transformMatch _ [_:everythingButUrl:url:[]] = do
  longUrl <- findUnshortenedUrl url
  return $ everythingButUrl ++ longUrl
transformMatch original _ = return original

-- When it matches:
-- matchAgainstUrlRegex "hello: http://g.co"
-- [["hello: http://g.co", "hello: ", "http://g.co"]]
--
-- If it doesn't match, it returns an empty list.
matchAgainstUrlRegex :: String -> ListOfMatches
matchAgainstUrlRegex = (=~ "(.+)(http://.*)$")

findUnshortenedUrl :: String -> IO String
findUnshortenedUrl shortenedUrl = do
  response <- simpleHTTP (getRequest shortenedUrl)
  let header = fmap (findHeader HdrLocation) response
  return $ unwrapWithFallback shortenedUrl header

unwrapWithFallback :: a -> Either b (Maybe a) -> a
unwrapWithFallback fallback = either (const fallback) (fromMaybe fallback)
