module Lang.JS0.AST
where

import Lang.JS0.Prelude
import Lang.JS0.BasicTypes

-- ----------------------------------------

data Stmt
  = VarStmt    Name Expr
  | FctStmt    Expr
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

mkFctStmt :: Expr -> Stmt
mkFctStmt = FctStmt

-- ----------------------------------------

data Expr
  = NumLit  Double
  | StrLit  Text
  | BoolLit Bool
  | ObjLit  [(Text, Expr)]
  | ArrLit  [Expr]
  | RELit   Text Text
  | FctLit  (Maybe Name) [Name] Stmt
  | NullLit
  | UndefLit
  | Ident   Name
  | Unary   UnaryOp Expr
  | Binary  BinaryOp Expr Expr
  | Cond    Expr Expr Expr
  | DotOp   Expr Name
  | BoxOp   Expr Expr
  | Call    Expr [Expr]
  deriving (Show)

mkNumLit :: Double -> Expr
mkNumLit = NumLit

mkStrLit :: Text -> Expr
mkStrLit = StrLit

mkObjLit :: [(Text, Expr)] -> Expr
mkObjLit = ObjLit

mkArrLit :: [Expr] -> Expr
mkArrLit = ArrLit

mkRegexpLit :: Text -> Text -> Expr
mkRegexpLit = RELit

mkFctLit :: Maybe Name -> [Name] -> Stmt -> Expr
mkFctLit = FctLit

mkNull :: Expr
mkNull = NullLit

mkUndef :: Expr
mkUndef = UndefLit

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

mkEQref :: Expr -> Expr -> Expr
mkEQref = Binary Beqr

mkNEref :: Expr -> Expr -> Expr
mkNEref = Binary Bner

mkAnd :: Expr -> Expr -> Expr
mkAnd = Binary Band

mkOr :: Expr -> Expr -> Expr
mkOr = Binary Bor

mkCond :: Expr -> Expr -> Expr -> Expr
mkCond = Cond

mkAssign :: Expr -> Expr -> Expr
mkAssign = Binary Bassign

mkIncr :: Expr -> Expr -> Expr
mkIncr = Binary Bincr

mkDecr :: Expr -> Expr -> Expr
mkDecr = Binary Bdecr

mkPreIncr :: Expr -> Expr
mkPreIncr = Unary PreIncr

mkPreDecr :: Expr -> Expr
mkPreDecr = Unary PreDecr

mkPostIncr :: Expr -> Expr
mkPostIncr = Unary PostIncr

mkPostDecr :: Expr -> Expr
mkPostDecr = Unary PostDecr

mkDelete :: Expr -> Expr
mkDelete = Unary Udelete

mkNew :: Expr -> Expr
mkNew = Unary Unew

mkTypeof :: Expr -> Expr
mkTypeof = Unary Utypeof

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
