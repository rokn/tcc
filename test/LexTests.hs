module LexTests where

import TccCore.Lexer
import TccCore.Keyword
import TccCore.Token

import Test.Tasty
import Test.Tasty.HUnit

data LexTest = LexTest {
    description :: String,
    code        :: String,
    tokens      :: [Token]
}



lexTests = [
    LexTest {
        description = "Minimal program",
        code = "int main(){}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            CloseBrace
         ]
    },
    LexTest {
        description = "Simple identifier",
        code = "int",
        tokens = [
            Identifier "int"
         ]
    },
    LexTest {
        description = "Main with return(No Literal)",
        code = "int main(){\nreturn;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Main with return(With Literal)",
        code = "int main(){\nreturn 2;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "2",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "MissingSpaces",
        code = "intmain(){return2;}",
        tokens = [
            Identifier "intmain",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            Identifier "return2",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Main with return(With Literal 0)",
        code = "int main(){\nreturn 0;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "0",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Main with return(With Literal 123456789)",
        code = "int main(){\nreturn 123456789;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "123456789",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Lotsa spaces",
        code = "    int    main    (  )  {\nreturn    37   ;    }  ",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "37",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Lotsa new lines",
        code = "\n\n\n\nint\n\n\n\nmain\n\n\n\n(\n\n\n\n) \
              \ \n\n\n\n{\nreturn\n\n\n\n37\n\n\n\n;\n\n\n\n}\n\n\n\n",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "37",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Tabs test",
        code = "int main() {\n\treturn 37;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "37",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "More tabs test",
        code = "\tint \t\tmain(\t) \t\t{\n\t\treturn \t37;\t\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "37",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Hex literals",
        code = "int main() {\n\treturn 0x37;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "0x37",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Hex literals 0",
        code = "int main() {\n\treturn 0x0;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "0x0",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Hex literals AF37",
        code = "int main() {\n\treturn 0xAF37;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "0xAF37",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Hex literals CAFECAFE",
        code = "int main() {\n\treturn 0xCAFECAFE;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "0xCAFECAFE",
            SemiColon,
            CloseBrace
         ]
    },
    LexTest {
        description = "Hex literals 00000",
        code = "int main() {\n\treturn 0x00000;\n}",
        tokens = [
            Identifier "int",
            Identifier "main",
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            KeywordToken Return,
            NumberLiteral "0x00000",
            SemiColon,
            CloseBrace
         ]
    }
    ]

testCases = map toTestCase lexTests

toTestCase lexTest =
    testCase (description lexTest) $
        assertEqual "Lexer should give correct tokens"
            (tokens lexTest)
            (tccLex $ code lexTest)

tests = testGroup "Lexer tests" testCases
