{-# LANGUAGE StandaloneDeriving #-}

module Lang.JS0.Instructions
where

import Lang.JS0.Prelude
import Lang.JS0.BasicTypes

-- ----------------------------------------
--
-- JS0 VM instruction setq

data JSInstr' disp
  = LoadBool   Bool
  | LoadNumber Number
  | LoadString Text
  | LoadNull
  | NewObject
  | NewFctObj  Name
  | LoadVar    Name
  | LoadField
  | StoreVar   Name
  | StoreField
  | OP1        UnaryOp
  | OP2        BinaryOp
  | Jump       disp
  | Branch     Bool disp
  | Label      disp
  | FctCall    Name ParCnt
  | MethodCall ParCnt
  | ConstrCall ParCnt

type JSInstr = JSInstr' Offset

type JSAssInstr = JSInstr' Lab

type Offset = Int

type Lab    = Text

type ParCnt = Int

-- ----------------------------------------

deriving instance Show disp => Show (JSInstr' disp)

-- ----------------------------------------
