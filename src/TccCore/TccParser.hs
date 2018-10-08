{-# LANGUAGE MonadComprehensions #-}
module TccCore.TccParser
    (
    prog
    ) where

import Parser
import TccCore.AST
import TccCore.Token
import TccCore.Keyword
import qualified TccCore.ParserErrors as PErr
import Control.Applicative ((<|>))

prog :: Parser Program
prog = [ Program functions | functions <- parse (many1 function)]

function :: Parser Function
function = [ Function (fName, fBlock) | _ <- symbol "int",
                                        fName <- identifier ["int", "return"],
                                        _ <- symbol "(",
                                        _ <- symbol ")",
                                        fBlock <- block ] <|> pFail "Expected a function"

block :: Parser Block
block = [ Block stmts | _ <- symbol "{",
                        stmts <- many statement,
                        _ <- symbol "}"] <|> pFail "Expected a block"

statement :: Parser Statement
statement = [ ReturnStatement e | _ <- returnStatement, e <- expr, _ <- symbol ";" ] <|> pFail "Expected a valid statement"

returnStatement = string "return" >> many1 spaces

expr :: Parser Expression
expr = (integer >>= \n -> return (Constant n)) <|> pFail "Expected a valid expression"
