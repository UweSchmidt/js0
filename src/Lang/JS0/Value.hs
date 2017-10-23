{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Lang.JS0.Value
where

import Lang.JS0.Prelude
import Lang.JS0.BasicTypes

import qualified Data.IntMap as IM
import qualified Data.Map    as M

-- ----------------------------------------

-- type of machine values of the JS0 virtual machine

data JSValue
  = JSRef       { _jsRef     :: ! Ref     }
  | JSString    { _jsString  :: ! Text    }
  | JSNumber    { _jsNumber  :: ! Number  }
  | JSBool      { _jsBool    :: ! Bool    }
  | JSUndefined
  | JSNull
  | JSCodeRef   { _jsCodeRef :: ! CodeRef   }  -- internal: reference of a code segment for a function
  | JSPc        { _jsPc      :: ! Int       }  -- internal: offset into code segment
  | JSValues    { _jsValues  :: ! [JSValue] }  -- internal: for eval stack


-- ----------------------------------------

-- object references, pointers into the global object store
-- internally represented as Int's

newtype Ref     = Ref     { unRef     :: Int }


-- code references, every function is compiled into a code segment
-- these segment are stored in the global CodeStore

newtype CodeRef = CodeRef { unCodeRef :: Name }

-- ----------------------------------------
--
-- operations on Ref and CodeRef

deriving instance Eq   Ref
deriving instance Ord  Ref
deriving instance Num  Ref
deriving instance Show Ref

instance Pretty Ref where
  pretty = show . unRef

deriving instance Eq   CodeRef
deriving instance Ord  CodeRef
deriving instance Show CodeRef

instance Pretty CodeRef where
  pretty = (^. isoString) . unCodeRef

-- ----------------------------------------
--
-- a JS object is a map from keys to JSValue's
-- There are 2 kinds of keys, the JavaScript keys represented by a string
-- and a few internal keys, which are only accessed within the VM

newtype JSObj
  = JSObj { _jsObj :: Map JSKey JSValue }

data JSKey
  = Hidden ! InternalKey
  | Name   ! Text

data InternalKey
  = KeyEnv          -- the closure object for access of global variables
  | KeyThis         -- the this object
  | KeyProto        -- the prototype chain
  | KeyCodeSeg      -- the code segement for current function
  | KeyPC           -- the program counter for current code segment
  | KeyEvalStack    -- the evaluation stack

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

jsValues :: Prism' JSValue [JSValue]
jsValues = prism
  JSValues
  (\ x -> case x of
            JSValues y -> Right y
            _          -> Left  x
  )

jsStack :: Prism' JSValue (JSValue, [JSValue])
jsStack = jsValues . _Cons

jsTOS :: Traversal' JSValue JSValue
jsTOS = jsStack . _1

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
jsObjAt' key = isoJSObj . at key

jsObjAt :: JSKey -> Lens' JSObj JSValue
jsObjAt key = jsObjAt' key . isoUndef
  where
    isoUndef = iso fromM toM
      where
        toM JSUndefined = Nothing
        toM x           = Just x
        fromM Nothing   = JSUndefined
        fromM (Just x)  = x

isoJSObj :: Iso' JSObj (Map JSKey JSValue)
isoJSObj = iso _jsObj JSObj

isoJSObjList :: Iso' JSObj [(JSKey, JSValue)]
isoJSObjList = isoJSObj . isoMapList

-- ----------------------------------------
--
-- the global object store consists of a map from Ref's to objects,
-- the map is implemented as an IntMap
-- for generating new refs, there is a newRef counter

data JSObjStore
  = JSObjStore
    { _jsObjStore :: ! (IM.IntMap JSObj)
    , _jsNewRef   :: ! Int
    }


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

-- ----------------------------------------
--
-- the object store lenses

jsObjStore :: Lens' JSObjStore (IM.IntMap JSObj)
jsObjStore k os =
  (\ new -> os {_jsObjStore = new}) <$> k (_jsObjStore os)

jsNewRef :: Lens' JSObjStore Ref
jsNewRef k os =
  (\ new -> os {_jsNewRef = unRef new}) <$> k (Ref $ _jsNewRef os)

jsObjStoreAt :: Ref -> Lens' JSObjStore JSObj
jsObjStoreAt ref =
  jsObjStore
  . at (unRef ref)
  . checkJust ("jsObjStoreAt: undefined ref " ++ show ref)

-- serialise/deserialize an object store
isoObjStoreList :: Iso' JSObjStore [(Ref, JSObj)]
isoObjStoreList = iso toL fromL
  where
    toL os =
      map (first Ref) $ IM.toList (os ^. jsObjStore)

    fromL xs = foldl' addPair emptyJSObjStore $ xs
      where
        addPair ! os (r, o) =
          os & jsObjStoreAt r .~ o
             & jsNewRef       %~ max r

-- ----------------------------------------
--
-- lenses for access of fields of an object

jsKeyField' :: Ref -> JSKey -> Lens' JSObjStore (Maybe JSValue)
jsKeyField' ref key = jsObjStoreAt ref . jsObjAt' key

-- access a field of an object, return JSUndefined

jsKeyField :: JSKey -> Ref -> Lens' JSObjStore JSValue
jsKeyField key ref = jsObjStoreAt ref . jsObjAt key
{-
jsNamedField :: Text -> Ref -> Lens' JSObjStore JSValue
jsNamedField name = jsField (Name name)

jsHiddenField :: InternalKey -> Ref -> Lens' JSObjStore JSValue
jsHiddenField intKey = jsField (Hidden intKey)

jsEnvField :: Ref -> Lens' JSObjStore JSValue
jsEnvField = jsHiddenField KeyEnv

jsThisField :: Ref -> Lens' JSObjStore JSValue
jsThisField = jsHiddenField KeyThis

jsProtoField :: Ref -> Lens' JSObjStore JSValue
jsProtoField = jsHiddenField KeyProto

jsCodeSegField :: Ref -> Lens' JSObjStore JSValue
jsCodeSegField = jsHiddenField KeyCodeSeg

jsPCField :: Ref -> Lens' JSObjStore JSValue
jsPCField = jsHiddenField KeyPC

jsEvalStackField :: Ref -> Lens' JSObjStore JSValue
jsEvalStackField = jsHiddenField KeyEvalStack
-}

-- ----------------------------------------

newtype JSCodeStore
  = JSCodeStore { _jsCodeStore :: Map CodeRef JSCodeSeg}

deriving instance Show JSCodeStore

emptyJSCodeStore :: JSCodeStore
emptyJSCodeStore
  = JSCodeStore
    { _jsCodeStore = mempty }

type JSCodeSeg = ()

jsCodeStore :: Lens' JSCodeStore (Map CodeRef JSCodeSeg)
jsCodeStore k cs =
  (\ new -> cs {_jsCodeStore = new}) <$> k (_jsCodeStore cs)


jsCodeStoreAt :: CodeRef -> Lens' JSCodeStore JSCodeSeg
jsCodeStoreAt ref =
  jsCodeStore
  . at ref
  . checkJust ("jsCodeStoreAt: undefined code ref " ++ show ref)

-- ----------------------------------------
--
-- a fromJust like lens with a configurable error message

checkJust :: String -> Iso' (Maybe a) a
checkJust msg = iso (fromMaybe (error msg)) Just
{-# INLINE checkJust #-}

-- ----------------------------------------
