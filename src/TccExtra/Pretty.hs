module TccExtra.Pretty
    (
    prettyAST
    ) where

import TccCore.AST

prettyAST :: Program -> String
prettyAST (Program decs) = concat $ map prettyDeclaration decs


prettyDeclaration decl =
    case decl of
      Function (fName, block) -> "Function " ++ fName ++ ":\n"++tabs 1++"body:\n" ++ prettyBlock block 2


prettyBlock (Block stmts) t = concat $ map (prettyStatement t) stmts

prettyStatement t (ReturnStatement expr) = tabs t ++ "RETURN " ++ prettyExpr t expr

prettyExpr t (Constant n) = '<' : (show n) ++ ">\n"

tabs n = take (n*4) (repeat ' ')
