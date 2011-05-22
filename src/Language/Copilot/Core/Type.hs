-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- | Defines Copilot types. Must be in its own module
-- as HeteroMap depends on HasType.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Core.Type
  ( module Data.Int
  , module Data.Word
  , Type (..)
  , ByteVector (..)
  , HasType (..)
  , Streamable
  , sizeOfType
  ) where

import Control.DeepSeq (NFData (..))
import Data.Int
import Data.Type.Equality (EqT (eqT), (:=:) (Refl))
import Data.Word
import TypeLevel.Number.Nat
import Unsafe.Coerce (unsafeCoerce)

data Type ∷ * → * where
  Bool   ∷ Type Bool
  Int8   ∷ Type Int8
  Int16  ∷ Type Int16
  Int32  ∷ Type Int32
  Int64  ∷ Type Int64
  Word8  ∷ Type Word8
  Word16 ∷ Type Word16
  Word32 ∷ Type Word32
  Word64 ∷ Type Word64
  Float  ∷ Type Float
  Double ∷ Type Double
  BVec   ∷ (Eq n, Nat n, Show n) ⇒ n → Type (ByteVector n)

deriving instance Eq (Type α)

deriving instance Show (Type α)

data ByteVector ∷ * → * where
  ByteVector ∷ Nat n ⇒ [Int8] → ByteVector n

deriving instance Eq (ByteVector n)

deriving instance Show (ByteVector n)

instance NFData (ByteVector n) where
  rnf (ByteVector xs) = rnf xs

class HasType α where typeOf ∷ Type α

instance HasType Bool   where typeOf = Bool
instance HasType Int8   where typeOf = Int8
instance HasType Int16  where typeOf = Int16
instance HasType Int32  where typeOf = Int32
instance HasType Int64  where typeOf = Int64
instance HasType Word8  where typeOf = Word8
instance HasType Word16 where typeOf = Word16
instance HasType Word32 where typeOf = Word32
instance HasType Word64 where typeOf = Word64
instance HasType Float  where typeOf = Float
instance HasType Double where typeOf = Double
instance (Eq n, Nat n, Show n) ⇒
  HasType (ByteVector n) where
    typeOf = BVec (undefined ∷ n)

instance EqT Type where
  eqT Bool   Bool   = Just Refl
  eqT Int8   Int8   = Just Refl
  eqT Int16  Int16  = Just Refl
  eqT Int32  Int32  = Just Refl
  eqT Int64  Int64  = Just Refl
  eqT Word8  Word8  = Just Refl
  eqT Word16 Word16 = Just Refl
  eqT Word32 Word32 = Just Refl
  eqT Word64 Word64 = Just Refl
  eqT Float  Float  = Just Refl
  eqT Double Double = Just Refl
  eqT (BVec n) (BVec k) | toInt n == -- Here were must "help" GHC...
                          (toInt k ∷ Integer) = unsafeCoerce Refl
  eqT _      _      = Nothing

class
  ( NFData α
  , Eq α
  , Show α
  , HasType α
  ) ⇒ Streamable α

instance Streamable Bool
instance Streamable Int8
instance Streamable Int16
instance Streamable Int32
instance Streamable Int64
instance Streamable Word8
instance Streamable Word16
instance Streamable Word32
instance Streamable Word64
instance Streamable Float
instance Streamable Double
instance (Eq n, Nat n, Show n) ⇒ Streamable (ByteVector n)

sizeOfType ∷ Type α → Int
sizeOfType t =
  case t of
    Bool   → 1
    Int8   → 1
    Int16  → 2
    Int32  → 4
    Int64  → 8
    Word8  → 1
    Word16 → 2
    Word32 → 4
    Word64 → 8
    Float  → 4
    Double → 8
    BVec n → (toInt n)
