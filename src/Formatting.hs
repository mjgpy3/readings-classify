module Formatting (
    formatAsJson
  ) where

import Classifications

formatAsJson :: Classifications -> String
formatAsJson c = concat [
  "{\n"
  ,"  \"title\": ", "\"", escapeQuotes $ removeNewlines $ title c ,"\"\n"
  ,"}"
  ]

escapeQuotes :: String -> String
escapeQuotes [] = []
escapeQuotes ('"':rest) = '\\':'"':escapeQuotes rest
escapeQuotes (x:rest) = x:escapeQuotes rest

removeNewlines :: String -> String
removeNewlines = filter (`notElem` "\n\r")

