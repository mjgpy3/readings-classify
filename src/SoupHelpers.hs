module SoupHelpers (
    getTagOpenName
    ,isHeader
    ,takeTillClose
    ,extractHeaderTags
  ) where

import Text.HTML.TagSoup

isHeader :: Tag String -> Bool
isHeader = (`elem` map Just possibleHeaderTagNames) . getTagOpenName

getTagOpenName :: Tag String -> Maybe String
getTagOpenName (TagOpen name _) = Just name
getTagOpenName _ = Nothing

getTagName :: Tag String -> Maybe String
getTagName (TagOpen name _) = Just name
getTagName (TagClose name) = Just name
getTagName _ = Nothing

possibleHeaderTagNames :: [String]
possibleHeaderTagNames = [[h, n] | h <- "hH", n <- "123456"]

takeTillClose :: Tag String -> [Tag String] -> [Tag String]
takeTillClose tag@(TagOpen name _) = (tag :) . go 1
  where
  go nOpenCount (t:ts) = t:case (nOpenCount, getTagName t == Just name, isTagClose t) of
    (1, True, True) -> []
    (n, True, True) -> go (n-1) ts
    (n, True, False) -> go (n+1) ts
    _ -> go nOpenCount ts
  go _ [] = []
takeTillClose _ = const []

extractHeaderTags :: [Tag String] -> [[Tag String]]
extractHeaderTags [] = []
extractHeaderTags (tag:tags) = (if isHeader tag then takeTillClose tag tags:rest else rest)
  where rest = extractHeaderTags tags
