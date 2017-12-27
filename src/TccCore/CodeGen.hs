module TccCore.CodeGen
    (
    tccGenerate
    ) where

import TccCore.AST

prefix  = ".text\n\
          \    .align 4\n"

tccGenerate :: Program -> String
tccGenerate (Program decs) = prefix ++ genDeclarations decs


genDeclarations [] = ""
genDeclarations (d:decs) = genDeclaration d ++ genDeclarations decs

genDeclaration dec =
    case dec of
      Function(fName, block) -> funcDecl fName ++ genBlock block


funcDecl name = spaces ++ ".globl " ++ name ++ "\n" ++ name ++ ":\n"

genBlock (Block stmts) = concat $ map genStatement stmts

genStatement (ReturnStatement expr) = genExpression expr ++ returnStmt

returnStmt = spaces ++ "ret\n"

genExpression (Constant numb) =
    spaces ++ "movl" ++ spaces ++
        "$" ++ (show numb) ++ ", %eax\n"


spaces = spacesN 1
spacesN n = take (n*4) (repeat ' ')
