module TccCore.Lexer
    (
    tccLex
    ) where

import TccCore.Token
import TccCore.Keyword
import TccCore.Helpers
import Text.Regex.Posix

splitters = "(){}; "

keywordRegexes = map (\k -> (k, getKeywordRegex k)) allKeywords

identifierPattern = "^[a-zA-Z]\\w*$"

literalPattern = "^(0x[0-9a-fA-F]+|[0-9]+)$"

type LexError = String

tccLex :: String -> [Token]
tccLex [] = []
tccLex code = lexSplitted (code `splitBy` splitters)

lexSplitted :: [String] -> [Token]
lexSplitted (" " : rest) = lexSplitted rest
lexSplitted (""  : rest) = lexSplitted rest
lexSplitted ("{" : rest) = OpenBrace        : lexSplitted rest
lexSplitted ("}" : rest) = CloseBrace       : lexSplitted rest
lexSplitted ("(" : rest) = OpenParenthesis  : lexSplitted rest
lexSplitted (")" : rest) = CloseParenthesis : lexSplitted rest
lexSplitted (";" : rest) = SemiColon        : lexSplitted rest
lexSplitted (lx  : rest)
  | isKeyword    = KeywordToken (Just keyword) : lexSplitted rest
  | isIdentifier = Identifier identifier       : lexSplitted rest
  | isLiteral    = NumberLiteral literal       : lexSplitted rest
  where (isKeyword, keyword)       = checkForKeyword lx
        (isIdentifier, identifier) = checkForIdentifier lx
        (isLiteral, literal)       = checkForLiteral lx

checkForKeyword lexeme = checkForKeyword' keywordRegexes
    where checkForKeyword' [] = (False, Nothing)
          checkForKeyword' ((keyword,pat):pats)
            | ((lexeme =~ pat)::Bool) = (True, Just keyword)
            | otherwise = checkForKeyword' pats

checkForIdentifier lexeme = ((lexeme =~ identifierPattern) :: Bool, lexeme)
checkForLiteral lexeme    = ((lexeme =~ literalPattern) :: Bool,    lexeme)
