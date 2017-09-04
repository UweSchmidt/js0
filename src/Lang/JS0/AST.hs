module Lang.JS0.AST
where

data Stmt = ExprStmt Expr
  deriving (Show)

data Expr
  = LitNum Double
  | LitString String
  | Null
  deriving (Show)
