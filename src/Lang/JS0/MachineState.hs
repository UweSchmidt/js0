{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lang.JS0.MachineState
where

import Control.Monad.State.Strict
import Control.Monad.IO.Class

import Lang.JS0.Prelude
import Lang.JS0.BasicTypes
import Lang.JS0.Value

-- ----------------------------------------

type Action a = StateT JSState IO a

runJS :: Action a -> JSState -> IO (a, JSState)
runJS m s0 = runStateT m s0

runJS0 :: Action a -> IO (a, JSState)
runJS0 m = runStateT m emptyJSState

execJS0 :: Action a -> IO ()
execJS0 m = runJS0 m >> return ()

-- ----------------------------------------

putLn :: String -> Action ()
putLn = liftIO . putStrLn

newObj :: Action Ref
newObj = do
  (os, ref) <- uses jsData newJSObj
  jsData .= os
  return ref

-- the basic access functions for objects

modifyKey :: Ref -> JSKey -> (JSValue -> JSValue) -> Action ()
modifyKey ref key fct = (jsData . jsKeyField key ref) %= fct

setKey :: Ref -> JSKey -> JSValue -> Action ()
setKey ref key val = (jsData . jsKeyField key ref) .= val

getKey :: Ref -> JSKey -> Action JSValue
getKey ref key = use (jsData . jsKeyField key ref)

-- specialized access functions

setField :: Ref -> Name -> JSValue -> Action ()
setField ref name = setKey ref (Name name)

setProto :: Ref -> JSValue -> Action ()
setProto ref = setKey ref (Hidden KeyProto)

-- ----------------------------------------

data JSState
  = JSState { _jsGORef :: ! Ref         -- ref to the global object
            , _jsRTS   :: ! [Ref]       -- stack of refs to the activation records
            , _jsCode  :: ! JSCodeStore -- "library" of code segments
            , _jsData  :: ! JSObjStore  -- heap: all JS  objects
            }

deriving instance Show JSState

emptyJSState :: JSState
emptyJSState
  = JSState
    { _jsGORef = ref0
    , _jsRTS   = mempty
    , _jsCode  = emptyJSCodeStore
    , _jsData  = cs0
    }
  where
    (cs0, ref0) = newJSObj emptyJSObjStore

-- ----------------------------------------

jsGORef :: Lens' JSState Ref
jsGORef k st =
  (\ new -> st {_jsGORef = new}) <$> k (_jsGORef st)

jsRTS :: Lens' JSState [Ref]
jsRTS k st =
  (\ new -> st {_jsRTS = new}) <$> k (_jsRTS st)

jsCode :: Lens' JSState JSCodeStore
jsCode k st =
  (\ new -> st {_jsCode = new}) <$> k (_jsCode st)

jsData :: Lens' JSState JSObjStore
jsData k st =
  (\ new -> st {_jsData = new}) <$> k (_jsData st)

-- ----------------------------------------
