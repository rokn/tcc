module ParseTests where

import TccCore.Parser
import TccCore.Keyword
import TccCore.Token
import TccCore.AST
import qualified TccCore.ParserErrors as PErr

import Test.Tasty
import Test.Tasty.HUnit

data ParseTest = ParseTest {
    description :: String,
    tokens      :: [Token],
    output     :: Either [PErr.ParseError] Program
}



parseTests = [
    ParseTest {
        description = "Minimal program",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            CloseBrace
         ],
         output = Right (Program
         [
            Function ("main", Block [])
         ])
    },
    ParseTest {
        description = "Main with return(No Literal)",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            SemiColon,
            CloseBrace
         ],
         output = Left [PErr.missingStatement]
    },
    ParseTest {
        description = "Main with return(With Literal)",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral 2,
            SemiColon,
            CloseBrace
         ],
         output = Right (Program
         [
            Function ("main", Block
            [
                ReturnStatement (Constant 2)
            ])
         ])

    },
    ParseTest {
        description = "Main with return(With Literal 0)",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral 0,
            SemiColon,
            CloseBrace
         ],
         output = Right (Program
         [
            Function ("main", Block
            [
                ReturnStatement (Constant 0)
            ])
         ])
    },
    ParseTest {
        description = "Main with return(With Literal 123456789)",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral 123456789,
            SemiColon,
            CloseBrace
         ],
         output = Right (Program
         [
            Function ("main", Block
            [
                ReturnStatement (Constant 123456789)
            ])
         ])
    },
    ParseTest {
        description = "Hex literals",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral 0x37,
            SemiColon,
            CloseBrace
         ],
         output = Right (Program
         [
            Function ("main", Block
            [
                ReturnStatement (Constant 0x37)
            ])
         ])
    },
    ParseTest {
        description = "Hex literals 0",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral 0x0,
            SemiColon,
            CloseBrace
         ],
         output = Right (Program
         [
            Function ("main", Block
            [
                ReturnStatement (Constant 0x0)
            ])
         ])
    },
    ParseTest {
        description = "Hex literals AF37",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral 0xAF37,
            SemiColon,
            CloseBrace
         ],
         output = Right (Program
         [
            Function ("main", Block
            [
                ReturnStatement (Constant 0xAF37)
            ])
         ])
    },
    ParseTest {
        description = "Empty file",
        tokens = [],
         output = Left [PErr.unexpectedEof]
    },
    ParseTest {
        description = "Missing function name",
        tokens = [
            Identifier "int",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral 0xAF37,
            SemiColon,
            CloseBrace
         ],
         output = Left [PErr.missingFuncDeclaration]
    },
    ParseTest {
        description = "Missing function open parenthesis",
        tokens = [
            Identifier "int",
            Identifier "main",
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral 0xAF37,
            SemiColon,
            CloseBrace
         ],
         output = Left [PErr.missingFuncDeclaration]
    },
    ParseTest {
        description = "Block missing open brace",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            KeywordToken Return,
            NumberLiteral 0xAF37,
            SemiColon,
            CloseBrace
         ],
         output = Left [PErr.missingOpenBrace]
    },
    ParseTest {
        description = "Missing return",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            NumberLiteral 0xAF37,
            SemiColon,
            CloseBrace
         ],
         output = Left [PErr.missingStatement]
    },
    ParseTest {
        description = "Missing closing brace",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral 0xAF37,
            SemiColon
         ],
         output = Left [PErr.missingCloseBrace]
    }
    ]

testCases = map toTestCase parseTests

toTestCase parseTest =
    testCase (description parseTest) $
        assertEqual "Parser should generate correct AST"
            (output parseTest)
            (tccParse $ tokens parseTest)

tests = testGroup "Parser tests" testCases
