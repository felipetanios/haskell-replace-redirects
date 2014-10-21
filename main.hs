module Main where

import Text.Regex.PCRE ((=~))
import Data.List (intercalate)
import Network.HTTP (simpleHTTP, getRequest, findHeader, HeaderName(HdrLocation))

type ListOfMatches = [[String]]

main :: IO ()
-- -- `a >>= b` takes the result of a and passes it to b, ~monadically~
main = getContents >>= transformBody >>= putStr

transformBody :: String -> IO String
transformBody body = do
  transformed <- transformLines $ lines body
  return $ unlines transformed

transformLines :: [String] -> IO [String]
transformLines body = sequence $ map transformLine body

-- Given a line *without* a URL, return the line unchanged.
-- Given a line *with* a URL, change the possibly-shortened URL to its longer
-- version.
transformLine :: String -> IO String
transformLine original
  | matchAgainstUrlRegex original == [] = return original
  | otherwise = do
      longUrl <- findUnshortenedUrl url
      return $ everythingButUrl ++ longUrl
    where
      everythingButUrl = match !! 1
      url = match !! 2
      match = head $ matchAgainstUrlRegex original

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
  return $ unwrapWithFallback header shortenedUrl

unwrapWithFallback :: Either b (Maybe a) -> a -> a
unwrapWithFallback wrapped fallback = case wrapped of
  Right (Just x) -> x
  _ -> fallback
