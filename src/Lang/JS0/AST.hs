module Lang.JS0.AST
where

import Data.Scientific
import Lang.JS0.Prelude

type Name = Text

data Stmt
  = VarStmt    Name Expr
  | LabelStmt  Name Stmt
  | ExprStmt   Expr
  | StmtSeq    [Stmt]
  | IfStmt     Expr Stmt Stmt
  | WhileStmt  Expr Stmt
  | DoStmt     Stmt Expr
  | TryStmt    Stmt Name Stmt
  | ThrowStmt  Expr
  | ReturnStmt (Maybe Expr)
  | BreakStmt  (Maybe Name)
  deriving (Show)

mkVarStmt :: Name -> Expr -> Stmt
mkVarStmt = VarStmt

mkLabelStmt :: Name -> Stmt -> Stmt
mkLabelStmt = LabelStmt

mkExprStmt :: Expr -> Stmt
mkExprStmt = ExprStmt

mkStmtSeq :: [Stmt] -> Stmt
mkStmtSeq = StmtSeq

mkIfStmt :: Expr -> Stmt -> Stmt -> Stmt
mkIfStmt = IfStmt

mkWhileStmt :: Expr -> Stmt -> Stmt
mkWhileStmt = WhileStmt

mkDoStmt :: Stmt -> Expr -> Stmt
mkDoStmt = DoStmt

mkTryStmt :: Stmt -> Name -> Stmt -> Stmt
mkTryStmt = TryStmt

mkThrowStmt :: Expr -> Stmt
mkThrowStmt = ThrowStmt

mkReturnStmt :: Maybe Expr -> Stmt
mkReturnStmt = ReturnStmt

mkBreakStmt :: Maybe Name -> Stmt
mkBreakStmt = BreakStmt

-- ----------------------------------------

data Expr
  = NumLit Scientific
  | StrLit Text
  | NullLit
  | Ident Name
  | Assign Expr Expr
  | IncrOp Expr Expr
  | DecrOp Expr Expr
  | DelOp Expr
  | DotOp Expr Name
  | BoxOp Expr Expr
  | Call  Expr [Expr]
  deriving (Show)

mkNumLit :: Scientific -> Expr
mkNumLit = NumLit

mkStrLit :: Text -> Expr
mkStrLit = StrLit

mkNull :: Expr
mkNull = NullLit

mkIdent :: Name -> Expr
mkIdent = Ident

mkAssign :: Expr -> Expr -> Expr
mkAssign = Assign

mkIncrExpr :: Expr -> Expr -> Expr
mkIncrExpr = IncrOp

mkDecrExpr :: Expr -> Expr -> Expr
mkDecrExpr = DecrOp

mkDelExpr :: Expr -> Expr
mkDelExpr = DelOp

mkDotExpr :: Expr -> Name -> Expr
mkDotExpr = DotOp

mkBoxExpr :: Expr -> Expr -> Expr
mkBoxExpr = BoxOp

mkCallExpr :: Expr -> [Expr] -> Expr
mkCallExpr = Call

isRefinement :: Expr -> Bool
isRefinement DotOp{} = True
isRefinement BoxOp{} = True
isRefinement _       = False

-- ----------------------------------------
