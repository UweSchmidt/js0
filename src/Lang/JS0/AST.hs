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
  | Ident  Name
  | Unary  UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Cond   Expr Expr Expr
  | DotOp  Expr Name
  | BoxOp  Expr Expr
  | Call   Expr [Expr]
  deriving (Show)

data UnaryOp
  = Udelete | Unew | Utypeof | Uplus | Uminus | Unot
  deriving (Eq, Ord, Show)

data BinaryOp
  = Bincr | Bdecr | Bassign
  | Bmult | Bdiv  | Brem
  | Badd  | Bsub
  | Bge   | Ble   | Bgr   | Bls
  | Beq   | Bne
  | Band  | Bor
  deriving (Eq, Ord, Show)

mkNumLit :: Scientific -> Expr
mkNumLit = NumLit

mkStrLit :: Text -> Expr
mkStrLit = StrLit

mkNull :: Expr
mkNull = NullLit

mkIdent :: Name -> Expr
mkIdent = Ident

mkMult :: Expr -> Expr -> Expr
mkMult = Binary Bmult

mkDiv :: Expr -> Expr -> Expr
mkDiv  = Binary Bdiv

mkRem :: Expr -> Expr -> Expr
mkRem  = Binary Brem

mkAdd :: Expr -> Expr -> Expr
mkAdd = Binary Badd

mkSub :: Expr -> Expr -> Expr
mkSub = Binary Bsub

mkGE :: Expr -> Expr -> Expr
mkGE = Binary Bge

mkGR :: Expr -> Expr -> Expr
mkGR = Binary Bgr

mkLE :: Expr -> Expr -> Expr
mkLE = Binary Ble

mkLS :: Expr -> Expr -> Expr
mkLS = Binary Bls

mkEQ :: Expr -> Expr -> Expr
mkEQ = Binary Beq

mkNE :: Expr -> Expr -> Expr
mkNE = Binary Bne

mkAnd :: Expr -> Expr -> Expr
mkAnd = Binary Band

mkOr :: Expr -> Expr -> Expr
mkOr = Binary Bor

mkCond :: Expr -> Expr -> Expr -> Expr
mkCond = Cond

mkAssign :: Expr -> Expr -> Expr
mkAssign = Binary Bassign

mkIncrExpr :: Expr -> Expr -> Expr
mkIncrExpr = Binary Bincr

mkDecrExpr :: Expr -> Expr -> Expr
mkDecrExpr = Binary Bdecr

mkDelExpr :: Expr -> Expr
mkDelExpr = Unary Udelete

mkNewExpr :: Expr -> Expr
mkNewExpr = Unary Unew

mkTypeofExpr :: Expr -> Expr
mkTypeofExpr = Unary Utypeof

mkUplus :: Expr -> Expr
mkUplus = Unary Uplus

mkUminus :: Expr -> Expr
mkUminus = Unary Uminus

mkNot :: Expr -> Expr
mkNot = Unary Unot

mkDotExpr :: Expr -> Name -> Expr
mkDotExpr = DotOp

mkBoxExpr :: Expr -> Expr -> Expr
mkBoxExpr = BoxOp

mkCallExpr :: Expr -> [Expr] -> Expr
mkCallExpr = Call

isLValue :: Expr -> Bool
isLValue Ident{} = True
isLValue e       = isRefinement e

isRefinement :: Expr -> Bool
isRefinement DotOp{} = True
isRefinement BoxOp{} = True
isRefinement _       = False

isInvocation :: Expr -> Bool
isInvocation Call{}  = True
isInvocation _       = False

-- ----------------------------------------
