module Main where

import qualified Data.Map.Strict as M

import System.Environment
import Text.HTML.TagSoup

data Classifications = Classifications {
    title :: String
  }

data TitleFactors = TitleFactors {
    headerText :: String
    ,occursOnce :: Bool
  }
  deriving Show

type HeaderContext = M.Map String Int

headerToTitleFactors :: [Tag String] -> HeaderContext -> TitleFactors
headerToTitleFactors headerTag counts = TitleFactors {
    headerText = innerText headerTag
    ,occursOnce = M.findWithDefault 0 tagName counts == 1
  }
    where
    (TagOpen tagName _:_) = headerTag

escapeQuotes :: String -> String
escapeQuotes [] = []
escapeQuotes ('"':rest) = '\\':'"':escapeQuotes rest
escapeQuotes (x:rest) = x:escapeQuotes rest

removeNewlines :: String -> String
removeNewlines = filter (`notElem` "\n\r")

isHeader :: Tag String -> Bool
isHeader (TagOpen (firstChar:secondChar:_) _) = firstChar `elem` "hH" && secondChar `elem` "123456"
isHeader _ = False

takeTillClose :: Tag String -> [Tag String] -> [Tag String]
takeTillClose tag@(TagOpen name _) tagsFollowing = tag:go 1 tagsFollowing
  where
  go 1 (endTag@(TagClose endName):rest) = if endName == name then [endTag] else endTag:go 1 rest
  go n (endTag@(TagClose endName):rest) = endTag:go (if endName == name then (n-1) else n) rest
  go n (openTag@(TagOpen openName _):rest) = openTag:go (if openName == name then (n+1) else n) rest
  go n (current:tags) = current:go n tags
  go _ [] = []
takeTillClose _ _ = []

extractHeaderTags :: [Tag String] -> [[Tag String]]
extractHeaderTags [] = []
extractHeaderTags (tag:tags) = (if isHeader tag then takeTillClose tag tags:rest else rest)
  where rest = extractHeaderTags tags

countHeaders :: [[Tag String]] -> HeaderContext
countHeaders ((TagOpen name _:_):headers) = 
  let 
    counts = countHeaders headers 
    n = maybe 1 (+ 1) (M.lookup name counts)
  in
    M.insert name n counts
countHeaders _ = M.empty

formatAsJson :: Classifications -> String
formatAsJson c = concat [
  "{\n"
  ,"  \"title\": ", "\"", escapeQuotes $ removeNewlines $ title c ,"\"\n"
  ,"}"
  ]

findTitle :: [[Tag String]] -> String
findTitle headers = innerText $ head headers
  where
  counts = countHeaders headers
  factors = map (`headerToTitleFactors` counts) headers

classify :: (Classifications -> String) -> String -> String
classify formatter rawHtml = formatter $ Classifications { title = findTitle headers }
  where
  tags = parseTags rawHtml
  headers = extractHeaderTags tags

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      text <- readFile path
      putStrLn $ classify formatAsJson text
    _ -> putStrLn "Usage: ./Classify <html-file>"
