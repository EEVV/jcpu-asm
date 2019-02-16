module Stmt where

data Expr = ExprEmpty
    | ExprTrue
    | ExprNum Int
    | ExprReg Int
    | ExprIden String
    | ExprNot Exprs
    | ExprNeg Exprs
    | ExprMem Exprs
    | ExprOr Exprs Exprs
    | ExprAnd Exprs Exprs
    | ExprXor Exprs Exprs
    | ExprEq Exprs Exprs
    | ExprLt Exprs Exprs
    | ExprGt Exprs Exprs
    | ExprAdd Exprs Exprs
    | ExprSub Exprs Exprs
    | ExprMul Exprs Exprs
    | ExprDiv Exprs Exprs deriving Show

type Exprs = [Expr]

data Stmt = StmtLabel String
    | StmtNum Int
    | StmtIden String
    | StmtSet Exprs Exprs Exprs deriving Show

type Stmts = [Stmt]