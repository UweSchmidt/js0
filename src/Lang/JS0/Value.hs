{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lang.JS0.Value
where

import Lang.JS0.Prelude
import Data.IntMap (IntMap)

import qualified Data.Map as M

-- ----------------------------------------

data JSValue
  = JSRef       { _jsRef    :: Ref    }
  | JSString    { _jsString :: Text   }
  | JSNumber    { _jsNumber :: Double }
  | JSBool      { _jsBool   :: Bool   }
  | JSUndefined
  | JSNull
  | JSCodeRef   { _jsCodeRef :: CodeRef }

newtype Ref     = Ref     { unRef     :: Int }

newtype CodeRef = CodeRef { unCodeRef :: Int }

newtype JSObj   = JSObj   { _jsObj    :: Map Text JSValue }

data JSObjStore
  = JSObjStore
    { _jsObjStore :: ! (IntMap JSObj)
    , _jsNewRef   :: ! Int
    }

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

-- ----------------------------------------
--
-- operations on Ref and CodeRef

deriving instance Eq   Ref
deriving instance Num  Ref
deriving instance Show Ref

deriving instance Eq   CodeRef
deriving instance Num  CodeRef
deriving instance Show CodeRef

-- ----------------------------------------

-- ----------------------------------------
--
-- operations on objects

deriving instance Show JSObj

instance Monoid JSObj where
  mempty = JSObj M.empty

  JSObj o1 `mappend` JSObj o2 =
    JSObj $ M.union o2 o1  -- right operand wins

-- lenses for JSObj

jsObjAt' :: Text -> Lens' JSObj (Maybe JSValue)
jsObjAt' key = isoJsObj . at key
  where
    isoJsObj = iso _jsObj JSObj

jsObjAt :: Text -> Lens' JSObj JSValue
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

-- access a field of an object, return Nothing if field is not there

jsField' :: Ref -> Text -> Lens' JSObjStore (Maybe JSValue)
jsField' ref key = jsObjStoreAt ref . jsObjAt' key

-- access a field of an object, return JSUndefined

jsField :: Ref -> Text -> Lens' JSObjStore JSValue
jsField ref key = jsObjStoreAt ref . jsObjAt key

-- ----------------------------------------
