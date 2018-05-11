{-# LANGUAGE MagicHash, MultiParamTypeClasses, TypeOperators, GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Data.PolyDict 
-- Copyright   : (c) Taku Terao, 2017 
-- License     : BSD3 
-- Maintainer  : autotaker@gmail.com 
-- Stability  : experimental 
-- Portability : GHC
-- Type-safe, polymorphic dictionary. 

module Data.PolyDict( DictValue, Assoc, Dict, Key, lookup, insert, access, access', empty) where

import Prelude hiding(lookup)

import Data.Aeson
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Kind(Constraint)
import Data.List(intersperse)
import Data.Proxy
import Data.Text (pack)

import Data.Type.Equality

import GHC.TypeLits
import GHC.Prim(Proxy#, proxy#)
import GHC.OverloadedLabels

import Lens.Micro
import Unsafe.Coerce

-- | 'DictValue' is the constraint for values can be inserted into 'Dict'
type family DictValue v :: Constraint where
    DictValue v = (Eq v, Show v, ToJSON v)

-- | 'Assoc' n k defines the type of value associated with key k.
--   Parameter n defines the namespace for dictionary fields. For example:
--   
-- > data Log
-- > type instance Assoc Log "argments" = [String]
-- > type instance Assoc Log "count" = Int
-- Then 'Dict' Log is a dictionary type with (at least) two fields "arguments" and "count".
--
-- One can access the fields by using 'insert' and 'lookup'.
--
-- >>> insert #count 0 (empty :: Dict Log)
-- {"count": 0}
-- >>> lookup #count (insert #count 0 (empty :: Dict Log))
-- Just 0
--
-- Or by using lenses:
--
-- >>> import Lens.Micro
-- >>> (empty :: Dict Log) & (access #count ?~ 1) . (access #arguments ?~ ["a","b","c"])
-- {"arguments": ["a","b","c"], "count": 1}
--
type family Assoc n (k :: Symbol)

-- | A polymorphic, type-safe dictinary type where the parameter 'n' represents the namespace of dictionary fields.
newtype Dict n = Dict (H.HashMap (Hashed String) (Entry n))
  deriving(Eq)

instance ToJSON (Dict n) where
    toJSON (Dict dict) = 
        object [ pack (symbolVal' k) .= toJSON v | (Entry k v) <- H.elems dict ]

instance Eq (Entry n) where
    Entry k1 v1 == Entry k2 v2 =
        case f k1 k2 of
            Just Refl -> v1 == v2
            Nothing -> False
        where
        f :: (KnownSymbol k1, KnownSymbol k2) => Proxy# k1 -> Proxy# k2 -> Maybe (k1 :~: k2)
        f _ _ = sameSymbol Proxy Proxy
    {-# INLINE (==) #-}


instance Show (Dict n) where
    showsPrec _ (Dict d) = 
        showChar '{' . 
           foldl1 (.) (intersperse (showString ", ") 
               [ shows (symbolVal' k) . showString ": " . shows v | (Entry k v) <- H.elems d ])
        . showChar '}'

data Entry n where
    Entry :: (KnownSymbol k, DictValue v, Assoc n k ~ v) => Proxy# k -> v -> Entry n

-- | The type of keys. With the OverloadedLabels extenstion, #foo is the key for field "foo"
--   
--
newtype Key (k :: Symbol) = Key (Proxy k)

instance k ~ k' => IsLabel k (Key k') where
    fromLabel = Key Proxy
    {-# INLINE fromLabel #-}

-- | Return the value associated with the key.
lookup :: (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> Dict n -> Maybe v
lookup key dict = dict ^. access key
{-# INLINE lookup #-}

-- | Insert the value at the specified key of the dictionary
insert :: (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> v -> Dict n -> Dict n
insert key value = access key ?~ value
{-# INLINE insert #-}

-- | Return the empty dictionary.
empty :: Dict n
empty = Dict H.empty
{-# INLINE empty #-}

-- | Give the lens accessing to the value associated with the key.
access :: forall n k v. (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> Lens' (Dict n) (Maybe v)
access key = lens getter setter
    where
    k = hashed (symbolVal key)
    getter (Dict dict) = case H.lookup k dict of
        Just (Entry _ v) -> Just (unsafeCoerce v)
        Nothing -> Nothing
    setter (Dict dict) Nothing = Dict $ H.delete k dict
    setter (Dict dict) (Just v) = Dict $ H.insert k (Entry (proxy# :: Proxy# k) v) dict
{-# INLINE access #-}

-- | Same as 'access' but requires the default value.
access' :: forall n k v. (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> v -> Lens' (Dict n) v
access' key def = access key . non def
{-# INLINE access' #-}

