module Main where

import System.Environment
import Text.HTML.TagSoup

data Classifications = Classifications {
    title :: String
  }

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
takeTillClose (TagOpen name _) tags = go 1 tags
  where
  go 1 (endTag@(TagClose endName):rest) = if endName == name then [endTag] else endTag:go 1 rest
  go n (endTag@(TagClose endName):rest) = endTag:go (if endName == name then (n-1) else n) rest
  go n (openTag@(TagOpen openName _):rest) = openTag:go (if openName == name then (n+1) else n) rest
  go n (tag:tags) = tag:go n tags
  go n [] = []
takeTillClose _ _ = []

extractHeaderTags :: [Tag String] -> [[Tag String]]
extractHeaderTags [] = []
extractHeaderTags (tag:tags) = (if isHeader tag then takeTillClose tag tags:rest else rest)
  where rest = extractHeaderTags tags

formatAsJson :: Classifications -> String
formatAsJson c = concat [
  "{\n"
  ,"  \"title\": ", "\"", escapeQuotes $ removeNewlines $ title c ,"\"\n"
  ,"}"
  ]

classify formatter rawHtml = formatter $ Classifications { title = (innerText $ head headers) }
  where
  headers = extractHeaderTags $ parseTags rawHtml

main = do
  args <- getArgs
  case args of
    [path] -> do
      text <- readFile path
      putStrLn $ classify formatAsJson text
    _ -> putStrLn "Usage: ./Classify <html-file>"
