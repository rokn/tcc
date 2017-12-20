module TccCore.Keyword where

data Keyword = Return
    deriving(Enum, Bounded, Show, Eq)

allKeywords = [(minBound :: Keyword) ..]

getKeywordRegex Return = "return"
