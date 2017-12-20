module TccCore.Lexer
    ( tccLex
    ) where

import TccCore.Token

tccLex :: String -> [Token]
tccLex program = [
    Identifier "int",
    Identifier "main",
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace
    ]

