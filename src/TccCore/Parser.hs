module TccCore.Parser
    (
    tccParse
    ) where

import TccCore.AST
import TccCore.Token
import TccCore.Keyword
import qualified TccCore.ParserErrors as PErr

tccParse :: [Token] -> Either [PErr.ParseError] Program
tccParse [] = Left [PErr.unexpectedEof]
tccParse tokens = parseDeclarations tokens
                    >>= \decs -> return $ Program decs


parseDeclarations :: [Token] -> Either [PErr.ParseError] [Function]
parseDeclarations tokens = parseDeclarations' tokens []
    where parseDeclarations' [] res     = Right (reverse res)
          parseDeclarations' tokens res = parseFunction tokens
                                            >>= \(func, tks) -> parseDeclarations' tks (func:res)


parseFunction :: [Token] -> Either [PErr.ParseError] (Function, [Token])
parseFunction ((Identifier fType):
               (Identifier fName):
                OpenParenthesis  :
                CloseParenthesis :
                rest) = parseBlock rest >>= \(block,tks) -> return $ (Function (fName, block), tks)
parseFunction _ = Left [PErr.missingFuncDeclaration]


parseBlock :: [Token] -> Either [PErr.ParseError] (Block, [Token])
parseBlock (OpenBrace:rest) = parseStatements rest
                                >>= \(stmts, tks) -> return $ (Block stmts, tks)
parseBlock _ = Left [PErr.missingOpenBrace]


parseStatements :: [Token] -> Either [PErr.ParseError] ([Statement], [Token])
parseStatements tokens = parseStatements' tokens []
    where parseStatements' [] res = Left [PErr.missingCloseBrace]
          parseStatements' (CloseBrace:rest) res = Right (reverse res, rest)
          parseStatements' ((KeywordToken Return):
                            (NumberLiteral numb) :
                             SemiColon : rest) res =
                                parseStatements' rest ((getReturn numb):res)
          parseStatements' _ _ = Left [PErr.missingStatement]
