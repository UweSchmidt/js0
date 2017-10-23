{-# LANGUAGE OverloadedStrings #-}
module Lang.JS0
       ( module Lang.JS0.AST
       , module Lang.JS0.Instructions
       , module Lang.JS0.MachineState
       , module Lang.JS0.Parser
       , module Lang.JS0.Prelude
       , module Lang.JS0.Value
       )
       where

import Lang.JS0.AST
import Lang.JS0.Instructions
import Lang.JS0.MachineState
import Lang.JS0.Parser
import Lang.JS0.Prelude
import Lang.JS0.Value

import Control.Monad.IO.Class
import Control.Monad.State.Strict

instance Pretty JSState where
  pretty x = concat $
             map (++ "\n") $
             [gor, rts, dat, cod]
    where
      gor = unlines
            [ "Global Ref: "
            , pretty (x ^. jsGORef)
            ]
      rts = unlines $
            ["Runtime Stack:"]
            ++
            (map pretty $ x ^. jsRTS)
      dat = unlines
            ["Objects:"
            , pretty (x ^. jsData)
            ]
      cod = unlines
            ["Code Segments:"
            , pretty (x ^. jsCode)
            ]

instance Pretty JSCodeStore

instance Pretty JSObjStore where
  pretty os = unlines $ map pretty1 (os ^. isoObjStoreList)
    where
      pretty1 (ref, obj) =
        pretty ref <> ": " <> pretty obj

instance Pretty JSObj where
  pretty o = "{ " <> intercalate "\n  , " os <> " }"
    where
      os =
        map (uncurry prettyPair) $ o ^. isoJSObjList

      prettyPair k v =
        pretty k <> ": " <> pretty v

instance Pretty JSValue
instance Pretty JSKey where
  pretty (Name   nm) = show nm
  pretty (Hidden hk) = show hk

dumpState :: Action ()
dumpState = do
  pretty <$> get >>= putLn

-- ----------------------------------------

t0 = execJS0 $ do
  putLn "Hello World"
  ref <- newObj
  setField ref "abc" (JSString "xyz")
  setProto ref (JSRef ref)
  dumpState
