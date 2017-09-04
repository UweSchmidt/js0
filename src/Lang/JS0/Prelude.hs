{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}


module Lang.JS0.Prelude
       ( ByteString
       , LazyByteString
       , Map
       , Set
       , Text
       , LazyText
       , Vector
       , IsEmpty(..)
       , IsString(..)
         -- Data.Aeson
       , ToJSON(..)
       , FromJSON(..)
       , (.=?!)
       , (.:?!)
         -- Text.Regex.XMLSchema.Generic
       , Regex
       , RegexText
       , parseRegex
       , parseRegex'
       , parseRegexExt
       , parseRegexExt'
       , match
       , matchRE
       , matchSubex
       , matchSubexRE
       , sed
       , sedExt
       , sedRE
       , tokenizeRE'
         -- Data.Maybe
       , fromMaybe
       , isNothing
       , isJust
       , listToMaybe
         -- Control.Monad
       , module Control.Applicative
       , module Control.Monad
         -- Data.List
       , intercalate
       , isPrefixOf
       , isSuffixOf
       , partition
       , sort
       , sortBy
       , nub
       , zip3
       , zip4
       , zip5
       , zip6
         -- Data.Char
       , module Data.Char
       , module Data.Foldable
       , module Data.Monoid
       , module Data.Void
         -- Data.Function
       , on
         -- System.FilePath
       , FilePath
       , (</>)
       , takeFileName
       , takeDirectory
         -- Control.Arrow
       , first, second, (&&&), (***)
         -- this module
       , compareBy
       , compareJust
       , compareJust'
       , partBy
         -- lens stuff
       , module Control.Lens
       , IsoString(..)
       , IsoText(..)
       , IsoInteger(..)
       , IsoHex(..)
       , IsoMaybe(..)
       , take1st
       , isoMapElems
       , isoMapList
       , isoSetList
       , isA
         -- utilities
       , (.||.)
       , partitionBy
       , divideAt
       , searchPos
       , removeAt
       , insertAt
         -- Monad ops
       , whenM
       , unlessM
       )
where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.Char
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Vector (Vector)
import           Data.Void
import           Numeric (readHex)
import           System.FilePath
import           Text.Printf (printf, PrintfArg)
import           Text.Regex.XMLSchema.Generic
import           Text.Regex.XMLSchema.Generic.RegexParser

type LazyByteString = LB.ByteString
type LazyText       = LT.Text

-- ----------------------------------------

class IsEmpty a where
  isempty :: a -> Bool


instance IsEmpty [a] where
  isempty = null
  {-# INLINE isempty #-}

instance IsEmpty (Maybe a) where
  isempty Nothing = True
  isempty _       = False
  {-# INLINE isempty #-}

instance IsEmpty Text where
  isempty = T.null
  {-# INLINE isempty #-}

instance IsEmpty ByteString where
  isempty = BS.null
  {-# INLINE isempty #-}

instance IsEmpty LazyByteString where
  isempty = LB.null
  {-# INLINE isempty #-}

instance IsEmpty (Set a) where
  isempty = S.null
  {-# INLINE isempty #-}

instance IsEmpty (Map k v) where
  isempty = M.null
  {-# INLINE isempty #-}

-- ----------------------------------------

class IsoString a where
  isoString :: Iso' a String

  default isoString :: (Read a, Show a) => Iso' a String
  isoString = iso show read
  {-# INLINE isoString #-}

instance IsoString String where
  isoString = iso id id
  {-# INLINE isoString #-}

instance IsoString Text where
  isoString = iso T.unpack T.pack
  {-# INLINE isoString #-}

instance IsoString LazyText where
  isoString = iso LT.unpack LT.pack
  {-# INLINE isoString #-}

instance IsoString ByteString where
  isoString = iso BU.toString BU.fromString
  {-# INLINE isoString #-}

instance IsoString LazyByteString where
  isoString = iso LBU.toString LBU.fromString
  {-# INLINE isoString #-}

instance IsoString Int

instance IsoString Integer

-- ----------------------------------------

class IsoText a where
  isoText :: Iso' a Text

instance IsoText Text where
  isoText = iso id id
  {-# INLINE isoText #-}

instance IsoText String where
  isoText = from isoString
  {-# INLINE isoText #-}

instance IsoText Int where
  isoText = isoString . isoText
  {-# INLINE isoText #-}

-- ----------------------------------------

class IsoInteger a where
  isoInteger :: Iso' a Integer

  default isoInteger :: (Integral a) => Iso' a Integer
  isoInteger = iso toInteger fromInteger
  {-# INLINE isoInteger #-}

-- ----------------------------------------

class IsoHex a where
  isoHex :: Iso' a String

  default isoHex :: (Integral a, PrintfArg a) => Iso' a String
  isoHex = iso toHex frHex
    where
      toHex   = printf "%016x"
      frHex s
        | [(i, "")] <- ps = i
        | otherwise       = 0
        where
          ps = readHex s

instance IsoHex Int

-- ----------------------------------------

class IsoMaybe a where
  isoMaybe :: Iso' a (Maybe a)

  default isoMaybe :: (IsEmpty a, Monoid a) => Iso' a (Maybe a)
  isoMaybe = iso toM fromM
    where
      toM xs
        | isempty xs = Nothing
        | otherwise  = Just xs
      fromM Nothing   = mempty
      fromM (Just xs) = xs
  {-# INLINE isoMaybe #-}

instance IsoMaybe String
instance IsoMaybe Text
instance IsoMaybe ByteString
instance IsoMaybe LazyByteString

take1st :: (IsoMaybe a, Monoid a) => [a] -> a
take1st xs = mconcat (map (^. isoMaybe) xs) ^. from isoMaybe

-- ----------------------------------------

-- an iso for converting a list of elemets into a map,
-- the key function extracts the keys of the elements

isoMapElems :: Ord k => (e -> k) -> Iso' (Map k e) [e]
isoMapElems key = iso M.elems (M.fromList . map (\ e -> (key e, e)))
{-# INLINE isoMapElems #-}

-- an iso for converting between maps and list of pairs

isoMapList :: Ord a => Iso' (Map a b) ([(a, b)])
isoMapList = iso M.toList M.fromList
{-# INLINE isoMapList #-}

isoSetList :: Ord a => Iso' (Set a) [a]
isoSetList = iso S.toList S.fromList
{-# INLINE isoSetList #-}

isA :: (a -> Bool) -> Prism' a a
isA p = prism id (\ o -> (if p o then Right else Left) o)
{-# INLINE isA #-}

{-
-- my first traversal, but Edward already made this, it's named each
-- works on various other structures

all3 :: Traversal (a, a, a) (b, b, b) a b
all3 inj (x1, x2, x3) = (,,) <$> inj x1 <*> inj x2 <*> inj x3
-- -}

-- ----------------------------------------
--
-- mothers little helper for en/decoding optional fileds

(.=?!) :: (ToJSON v, IsEmpty v) =>
          Text -> v -> [J.Pair]
t .=?! x
  | isempty x = []
  | otherwise = [t J..= x]
{-# INLINE (.=?!) #-}

(.:?!) :: (FromJSON v, Monoid v) =>
          J.Object -> Text -> J.Parser v
o .:?! t =
  o J..:? t J..!= mempty
{-# INLINE (.:?!) #-}

-- ----------------------------------------

infixr 2 .||.

-- | Lift boolean 'or' over predicates.

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p .||. q = \ v -> p v || q v


-- | group a list of entries by a mapping the
-- elements to an ordered domain

partitionBy :: (Ord e) => (a -> e) -> [a] -> [[a]]
partitionBy f =
  M.elems
  . foldr (\ x m -> M.insertWith (++) (f x) [x]
                    m) M.empty

-- | divide a list into equal length parts

divideAt :: Int -> [a] -> [[a]]
divideAt n
  | n <= 1    = map (:[])
  | otherwise = go
  where
    go [] = []
    go xs = ys : go xs'
      where
        (ys, xs') = splitAt n xs

-- search the position of the first element for which the predicate holds

searchPos :: (a -> Bool) -> [a] -> Maybe Int
searchPos p =
  listToMaybe . map fst . filter (p . snd) . zip [0..]


-- remove an element at a given index

removeAt :: Int -> [a] -> [a]
removeAt 0 xs = drop 1 xs
removeAt i xs
  | i < 0 = xs
removeAt _ [] = []
removeAt i (x : xs) = (x :) $! removeAt (i - 1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt _ x []        =  x : []
insertAt i x xs
  | i <= 0             =  x : xs
insertAt i x (x1 : xs) = (x1 :) $! insertAt (i - 1) x xs

-- ----------------------------------------

-- put all elemnts of a, which have equal e values
-- into a sublist
--
-- partBy (`mod` 3) [0..9] = [[0,3,6,9],[1,4,7],[2,5,8]]

partBy :: (Ord e) => (a -> e) -> [a] -> [[a]]
partBy f =
  M.elems
  . foldr (\ x m -> M.insertWith (++) (f x) [x]
                    m) M.empty

-- ----------------------------------------

-- compare 2 values with a sequence of compare functions
-- first res /= EQ wins

compareBy :: [a -> a -> Ordering] -> a -> a -> Ordering
compareBy fs x1 x2 =
  mconcat $ map (\ cmp -> cmp x1 x2) fs
{-# INLINE compareBy #-}


-- compare only on Just values
--
-- useful in compareBy when Nothing values occur

compareJust :: Ord a => Maybe a -> Maybe a -> Ordering
compareJust (Just x1) (Just x2) = compare x1 x2
compareJust _         _         = EQ
{-# INLINE compareJust #-}


-- compare with Nothing as largest values
--
-- default with compare: Nothing is smallest

compareJust' :: Ord a => Maybe a -> Maybe a -> Ordering
compareJust' (Just x1) (Just x2) = compare x1 x2
compareJust' (Just _ ) _         = LT
compareJust' _         (Just _ ) = GT
compareJust' _         _         = EQ
{-# INLINE compareJust' #-}

-- ----------------------------------------

whenM :: (Monoid a, Monad m) => m Bool -> m a -> m a
whenM b c = do
  b' <- b
  if b' then c else return mempty

{-# INLINE whenM #-}

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b c = do
  b' <- b
  unless b' c
{-# INLINE unlessM #-}

-- ----------------------------------------
