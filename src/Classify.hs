module Main where

import qualified Data.Map.Strict as M

import Classifications
import Formatting
import SoupHelpers
import System.Environment
import Text.HTML.TagSoup

type HeaderContext = M.Map String Int

headerToTitleFactors :: [Tag String] -> HeaderContext -> TitleFactors
headerToTitleFactors headerTag counts = TitleFactors {
    headerText = innerText headerTag
    ,occursOnce = M.findWithDefault 0 tagName counts == 1
  }
    where
    (TagOpen tagName _:_) = headerTag

countHeaders :: [[Tag String]] -> HeaderContext
countHeaders ((TagOpen name _:_):headers) = 
  let 
    counts = countHeaders headers 
    n = maybe 1 (+ 1) (M.lookup name counts)
  in
    M.insert name n counts
countHeaders _ = M.empty

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
