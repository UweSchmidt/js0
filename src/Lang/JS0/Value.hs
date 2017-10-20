{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lang.JS0.Value
where

import Lang.JS0.Prelude
import Lang.JS0.BasicTypes

import Data.IntMap (IntMap)

import qualified Data.Map as M

-- ----------------------------------------

-- type of machine values of the JS0 virtual machine

data JSValue
  = JSRef       { _jsRef     :: Ref     }
  | JSString    { _jsString  :: Text    }
  | JSNumber    { _jsNumber  :: Number  }
  | JSBool      { _jsBool    :: Bool    }
  | JSUndefined
  | JSNull
  | JSCodeRef   { _jsCodeRef :: CodeRef }  -- internal: reference of a code segment for a function
  | JSPc        { _jsPc      :: Int     }  -- internal: offset into code segment


-- object references, pointers into the global object store
-- internally represented as Int's

newtype Ref     = Ref     { unRef     :: Int }


-- code references, every function is compiled into a code segment
-- these segment are stored in the global CodeStore

newtype CodeRef = CodeRef { unCodeRef :: Name }

-- ----------------------------------------
--
-- a JS object is a map from keys to JSValue's
-- There are 2 kinds of keys, the JavaScript keys represented by a string
-- and a few internal keys, which are only accessed within the VM

newtype JSObj   = JSObj   { _jsObj    :: Map JSKey JSValue }

data JSKey
  = Name   Text
  | Hidden InternalKey

data InternalKey
                = KeyEnv | KeyCodeSeg | KeyThis | KeyPC | KeyProto

-- ----------------------------------------
--
-- the global object store consists of a map from Ref's to objects,
-- the map is implemented as an IntMap
-- for generating new refs, there is a newRef counter

data JSObjStore
  = JSObjStore
    { _jsObjStore :: ! (IntMap JSObj)
    , _jsNewRef   :: ! Int
    }


-- ----------------------------------------
-- operations on object keys

deriving instance Eq   InternalKey
deriving instance Ord  InternalKey
deriving instance Show InternalKey

deriving instance Eq   JSKey
deriving instance Ord  JSKey
deriving instance Show JSKey

-- ----------------------------------------
--
-- operations on values

deriving instance Show JSValue

-- prisms for JSValue

jsRef :: Prism' JSValue Ref
jsRef = prism
  JSRef
  (\ x -> case x of
            JSRef y -> Right y
            _       -> Left  x
  )

jsString :: Prism' JSValue Text
jsString = prism
  JSString
  (\ x -> case x of
            JSString y -> Right y
            _          -> Left  x
  )

jsNumber :: Prism' JSValue Double
jsNumber = prism
  JSNumber
  (\ x -> case x of
            JSNumber y -> Right y
            _          -> Left  x
  )

jsBool :: Prism' JSValue Bool
jsBool = prism
  JSBool
  (\ x -> case x of
            JSBool y -> Right y
            _        -> Left  x
  )

jsUndefined :: Prism' JSValue ()
jsUndefined = prism
  (const JSUndefined)
  (\ x -> case x of
            JSUndefined -> Right ()
            _           -> Left  x
  )

jsNull :: Prism' JSValue ()
jsNull = prism
  (const JSNull)
  (\ x -> case x of
            JSNull -> Right ()
            _      -> Left  x
  )

jsCodeRef :: Prism' JSValue CodeRef
jsCodeRef = prism
  JSCodeRef
  (\ x -> case x of
            JSCodeRef y -> Right y
            _           -> Left  x
  )

jsPc :: Prism' JSValue Int
jsPc = prism
  JSPc
  (\ x -> case x of
            JSPc y -> Right y
            _      -> Left  x
  )

-- ----------------------------------------
--
-- operations on Ref and CodeRef

deriving instance Eq   Ref
deriving instance Num  Ref
deriving instance Show Ref

deriving instance Eq   CodeRef
deriving instance Ord  CodeRef
deriving instance Show CodeRef

-- ----------------------------------------
--
-- operations on objects

deriving instance Show JSObj

instance Monoid JSObj where
  mempty = JSObj M.empty

  JSObj o1 `mappend` JSObj o2 =
    JSObj $ M.union o2 o1  -- right operand wins

-- lenses for JSObj

jsObjAt' :: JSKey -> Lens' JSObj (Maybe JSValue)
jsObjAt' key = isoJsObj . at key
  where
    isoJsObj = iso _jsObj JSObj

jsObjAt :: JSKey -> Lens' JSObj JSValue
jsObjAt key = jsObjAt' key . isoUndef
  where
    isoUndef = iso fromM toM
      where
        toM JSUndefined = Nothing
        toM x           = Just x
        fromM Nothing   = JSUndefined
        fromM (Just x)  = x

-- ----------------------------------------
--
-- the object store functions and lenses

deriving instance Show JSObjStore

emptyJSObjStore :: JSObjStore
emptyJSObjStore
  = JSObjStore
    { _jsObjStore = mempty
    , _jsNewRef   = 1
    }

newJSObj :: JSObjStore -> (JSObjStore, Ref)
newJSObj os =
  (newOs, newRef)
  where
    newRef = os ^. jsNewRef
    newOs  = os & jsObjStoreAt newRef .~ mempty
                & jsNewRef            +~ 1

jsObjStore :: Lens' JSObjStore (IntMap JSObj)
jsObjStore k os =
  (\ new -> os {_jsObjStore = new}) <$> k (_jsObjStore os)

jsNewRef :: Lens' JSObjStore Ref
jsNewRef k os =
  (\ new -> os {_jsNewRef = unRef new}) <$> k (Ref $ _jsNewRef os)

jsObjStoreAt :: Ref -> Lens' JSObjStore JSObj
jsObjStoreAt ref =
  jsObjStore . at (unRef ref) . checkJust ("jsObjStoreAt: undefined ref " ++ show ref)
  where
    checkJust :: String -> Iso' (Maybe a) a
    checkJust msg = iso (fromMaybe (error msg)) Just
    {-# INLINE checkJust #-}

-- ----------------------------------------
--
-- lenses for access of fields of an object

jsField' :: Ref -> JSKey -> Lens' JSObjStore (Maybe JSValue)
jsField' ref key = jsObjStoreAt ref . jsObjAt' key

-- access a field of an object, return JSUndefined

jsField :: JSKey -> Ref -> Lens' JSObjStore JSValue
jsField key ref = jsObjStoreAt ref . jsObjAt key

jsNamedField :: Text -> Ref -> Lens' JSObjStore JSValue
jsNamedField name = jsField (Name name)

jsHiddenField :: InternalKey -> Ref -> Lens' JSObjStore JSValue
jsHiddenField intKey = jsField (Hidden intKey)

jsThisField :: Ref -> Lens' JSObjStore JSValue
jsThisField = jsHiddenField KeyThis

jsProtoField :: Ref -> Lens' JSObjStore JSValue
jsProtoField = jsHiddenField KeyProto

jsEnvField :: Ref -> Lens' JSObjStore JSValue
jsEnvField = jsHiddenField KeyEnv

jsPCField :: Ref -> Lens' JSObjStore JSValue
jsPCField = jsHiddenField KeyPC

jsCodeSegField :: Ref -> Lens' JSObjStore JSValue
jsCodeSegField = jsHiddenField KeyCodeSeg


-- ----------------------------------------
