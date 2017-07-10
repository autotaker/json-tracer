{-# LANGUAGE MagicHash, MultiParamTypeClasses, TypeOperators #-}
module PolyDict( DictValue, Assoc, Dict, Key, lookup, insert, access, access', empty) where

import qualified Data.HashMap.Strict as H
import Data.Aeson
import Data.Kind(Constraint)
import Prelude hiding(lookup)
import GHC.TypeLits
import Data.Proxy
import Data.Hashable
import Unsafe.Coerce
import Data.Text (pack)
import Lens.Micro
import Data.Type.Equality


import GHC.Prim(Proxy#, proxy#)
import GHC.OverloadedLabels
import Data.List(intersperse)

type family DictValue v :: Constraint where
    DictValue v = (Eq v, Show v, ToJSON v)

type family Assoc n (k :: Symbol)

newtype Dict n = Dict (H.HashMap (Hashed String) (Entry n))

instance ToJSON (Dict n) where
    toJSON (Dict dict) = object [ pack (symbolVal' k) .= toJSON v | (Entry k v) <- H.elems dict ]

instance Eq (Entry n) where
    Entry k1 v1 == Entry k2 v2 =
        case f k1 k2 of
            Just Refl -> v1 == v2
            Nothing -> False
        where
        f :: (KnownSymbol k1, KnownSymbol k2) => Proxy# k1 -> Proxy# k2 -> Maybe (k1 :~: k2)
        f _ _ = sameSymbol Proxy Proxy

instance Eq (Dict n) where
    Dict d1 == Dict d2 = d1 == d2

instance Show (Dict n) where
    showsPrec _ (Dict d) = 
        showChar '{' . 
           foldl1 (.) (intersperse (showString ", ") [ shows (symbolVal' k) . showString ": " . shows v | (Entry k v) <- H.elems d ])
        . showChar '}'

data Entry n where
    Entry :: (KnownSymbol k, DictValue v, Assoc n k ~ v) => Proxy# k -> v -> Entry n

newtype Key (k :: Symbol) = Key (Proxy k)

instance k ~ k' => IsLabel k (Key k') where
    fromLabel _ = Key Proxy

lookup :: (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> Dict n -> Maybe v
lookup key dict = dict ^. access key

insert :: (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> v -> Dict n -> Dict n
insert key value = access key ?~ value

empty :: Dict n
empty = Dict H.empty

access :: forall n k v. (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> Lens' (Dict n) (Maybe v)
access key = lens getter setter
    where
    k = hashed (symbolVal key)
    getter (Dict dict) = case H.lookup k dict of
        Just (Entry _ v) -> Just (unsafeCoerce v)
        Nothing -> Nothing
    setter (Dict dict) Nothing = Dict $ H.delete k dict
    setter (Dict dict) (Just v) = Dict $ H.insert k (Entry (proxy# :: Proxy# k) v) dict

access' :: forall n k v. (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> v -> Lens' (Dict n) v
access' key def = access key . non def

