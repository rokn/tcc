module TccCore.Parser
    (
    tccParse,
    ParseError
    ) where

import TccCore.AST
import TccCore.Token
import TccCore.Keyword

type ParseError = String

-- data Expectation =
--     Single Token
--   | Multiple [Expectation]

-- ExpressionExpect = Single NumberLiteral

tccParse :: [Token] -> Either [ParseError] Program
tccParse = undefined
-- tccParse tokens = case getDeclarations tokens of
--                     Left errors        -> Left errors
--                     Right declarations -> Right (Program declarations)

-- getDeclarations tokens = getDeclarations' tokens []
--     where getDeclarations' [] decs = Right decs
--           getDeclarations' (t:ts) decs =
--               case t of
--                 Identifier "int" -> (getFunction ts):decs
--                 _                -> Left ["Expected type identifier"]

getFunction ((Identifier fType):
             (Identifier fName):
              OpenParenthesis  :
              CloseParenthesis :
              rest) = getBlock rest >>= \(block,tks) -> return $ (Function (fName, block), tks)

getFunction _ = Left ["Expected function declaration"]



getBlock (OpenBrace:rest) = parseStatements rest
                                >>= \(stmts, tks) -> return $ (Block stmts, tks)
getBlock _ = Left ["Expected an opening brace"]



parseStatements tokens = parseStatements' tokens []
    where parseStatements' [] res = Left ["Expected a closing brace"]
          parseStatements' (CloseBrace:rest) res = Right (reverse res, rest)
          parseStatements' ((KeywordToken Return):
                            (NumberLiteral numb) :
                            SemiColon            :
                            rest) res = parseStatements' rest ((getReturn numb):res)
