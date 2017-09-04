module Lang.JS0.AST
where

import Data.Scientific
import Lang.JS0.Prelude

data Stmt = ExprStmt Expr
  deriving (Show)

-- ----------------------------------------

data Expr
  = NumLit Scientific
  | StrLit Text
  | NullLit
  deriving (Show)

mkNumLit :: Scientific -> Expr
mkNumLit = NumLit

mkStrLit :: Text -> Expr
mkStrLit = StrLit

mkNull :: Expr
mkNull = NullLit

-- ----------------------------------------
