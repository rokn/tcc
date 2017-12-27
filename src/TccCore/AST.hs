module TccCore.AST where

data Program = Program [Function]
    deriving(Show, Eq)

type FunctionName = String
-- type ArgumentList = [Argument]
-- data Function = Function(FunctionName, ArgumentList, Block)
data Function = Function(FunctionName, Block)
    deriving(Show, Eq)

data Block = Block [Statement]
    deriving(Show, Eq)

data Statement = ReturnStatement Expression
    deriving(Show, Eq)

data Expression = Constant Integer
    deriving(Show, Eq)

getReturn n = ReturnStatement (Constant n)
