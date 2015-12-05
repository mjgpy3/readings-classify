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

isHeader :: Tag String -> Bool
isHeader (TagOpen (firstChar:secondChar:_) _) = firstChar `elem` "hH" && secondChar `elem` "123456"
isHeader _ = False

extractHeaderTags :: [Tag String] -> [Tag String]
extractHeaderTags = filter isHeader

formatAsJson :: Classifications -> String
formatAsJson c = concat [
  "{\n"
  ,"  \"title\": ", "\"", escapeQuotes $ title c ,"\"\n"
  ,"}"
  ]

classify formatter rawHtml = formatter $ Classifications { title = "Foo" }

main = do
  args <- getArgs
  case args of
    [path] -> do
      text <- readFile path
      putStrLn $ classify formatAsJson text
    _ -> putStrLn "Usage: ./Classify <html-file>"
