module Lang.JS0.BasicTypes where

import Lang.JS0.Prelude

-- ----------------------------------------
--
-- alias names

type Name   = Text

type Number = Double


data UnaryOp
  = Udelete | Unew | Utypeof | Uplus | Uminus | Unot
  | PreIncr | PreDecr | PostIncr | PostDecr
  deriving (Eq, Ord, Show)

data BinaryOp
  = Bincr | Bdecr | Bassign
  | Bmult | Bdiv  | Brem
  | Badd  | Bsub
  | Bge   | Ble   | Bgr   | Bls
  | Beq   | Bne   | Beqr  | Bner
  | Band  | Bor
  deriving (Eq, Ord, Show)

-- ----------------------------------------
