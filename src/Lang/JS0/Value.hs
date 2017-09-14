{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lang.JS0.Value
where

import Lang.JS0.Prelude
import Data.IntMap (IntMap)

-- ----------------------------------------

data JSValue
  = JSRef       { _jsRef    :: Ref    }
  | JSString    { _jsString :: Text   }
  | JSNumber    { _jsNumber :: Double }
  | JSBool      { _jsBool   :: Bool   }
  | JSUndefined
  | JSNull
  | JSCodeRef   { _jsCodeRef :: CodeRef }
  deriving (Show)

newtype Ref = Ref { unRef :: Int }
  deriving (Eq, Num, Show)

newtype CodeRef = CodeRef { unCodeRef :: Int }
  deriving (Eq, Num, Show)

newtype JSObj = JSObj { _jsObj :: Map Text JSValue }
  deriving (Show)

newtype JSObjStore = JSObjStore { _jsObjStore :: IntMap JSObj }
  deriving (Show)

-- ----------------------------------------

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
