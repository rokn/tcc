module TccCore.Token (Token(..))  where

import TccCore.Keyword

data Token =
        OpenBrace
      | CloseBrace
      | OpenParenthesis
      | CloseParenthesis
      | SemiColon
      | Identifier String
      | KeywordToken Keyword
      | Literal String
      deriving(Show, Eq)
