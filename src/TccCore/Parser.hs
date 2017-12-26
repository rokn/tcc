module TccCore.Parser
    (
    tccParse,
    ParseError,
    parseFunction
    ) where

import TccCore.AST
import TccCore.Token
import TccCore.Keyword

type ParseError = String

tccParse :: [Token] -> Either [ParseError] Program
tccParse [] = Left ["Unexpected EOF"]
tccParse tokens = parseDeclarations tokens
                    >>= \decs -> return $ Program decs


parseDeclarations :: [Token] -> Either [ParseError] [Function]
parseDeclarations tokens = parseDeclarations' tokens []
    where parseDeclarations' :: [Token] -> [Function] -> Either [ParseError] [Function]
          parseDeclarations' [] res     = Right (reverse res)
          parseDeclarations' tokens res = parseFunction tokens
                                            >>= \(func, tks) -> parseDeclarations' tks (func:res)


parseFunction :: [Token] -> Either [ParseError] (Function, [Token])
parseFunction ((Identifier fType):
               (Identifier fName):
                OpenParenthesis  :
                CloseParenthesis :
                rest) = parseBlock rest >>= \(block,tks) -> return $ (Function (fName, block), tks)

parseFunction _ = Left ["Expected function declaration"]



parseBlock (OpenBrace:rest) = parseStatements rest
                                >>= \(stmts, tks) -> return $ (Block stmts, tks)
parseBlock _ = Left ["Expected an opening brace"]



parseStatements tokens = parseStatements' tokens []
    where parseStatements' [] res = Left ["Expected a closing brace"]
          parseStatements' (CloseBrace:rest) res = Right (reverse res, rest)
          parseStatements' ((KeywordToken Return):
                            (NumberLiteral numb) :
                            SemiColon            :
                            rest) res = parseStatements' rest ((getReturn numb):res)
