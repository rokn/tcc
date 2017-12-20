module TccCore.Helpers where

type Splitters = String

splitBy :: String -> Splitters -> [String]
splityBy [] _ = []
splitBy str splits = splitBy' (span findSplitter str)
    where findSplitter = not . ((flip elem) splits)
          splitBy' (splitted, splitter:rest) =
              splitted : [splitter] : splitBy rest splits
          splitBy' (_, []) = []


