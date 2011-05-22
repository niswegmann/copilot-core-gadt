-- © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution
-- 3.0 Unported License: http://creativecommons.org/licenses/by/3.0

-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Core
  ( module Language.Copilot.Core.Type
  , Spec (..)
  , WithSpec (..)
  , Trig (..)
  , Strm (..)
  , Expr (..)
  , Op1 (..)
  , Op2 (..)
  , Op3 (..)
  ) where

import Language.Copilot.Core.HeteroMap (HeteroMap, Key)
import Language.Copilot.Core.Type

-- | A Copilot specification.
data Spec ∷ ((* → *) → *) → * → * where
  Spec
    ∷ (HasType α, HeteroMap map)
     ⇒ map (Strm (Key map))
     → Expr (Key map) α
     → Spec map α

data WithSpec α = WithSpec
  ( ∀ β .
      (
        ∀ map . HeteroMap map ⇒ Spec map α → β
      ) → β
  )

-- | A trigger.
data Trig ref where
  Trig
    ∷ Streamable α
     ⇒ String
     → Expr ref α
     → Trig ref

-- | A canonized stream.
data Strm ∷ (* → *) → * → * where
  Strm
    --
    ∷ (Streamable α, Eq (ref α), Ord (ref α), Show (ref α))
     ⇒ Type α
     → [α]
     → Expr ref α
     → Strm ref α

deriving instance Show (Strm ref α)

-- | An expressions is non-recursive.
data Expr ∷ (* → *) → * → * where
  Const
    ∷ Streamable α
     ⇒ α
     → Expr ref α
  -- The temporal look-ahead operator;
  -- It always points to a canonized stream.
  Drop
    ∷ (Streamable α, Eq (ref α), Ord (ref α), Show (ref α))
     ⇒ Int
     → ref α
     → Expr ref α
  Extern -- An external variable
    ∷ Streamable α
     ⇒ String
     → Expr ref α
  Op1 -- An unary operator.
    ∷ (Streamable α, Streamable β)
     ⇒ Op1 α β
     → Expr ref α
     → Expr ref β
  Op2 -- A binary operator.
    ∷ (Streamable α, Streamable β, Streamable γ)
    ⇒ Op2 α β γ
    → Expr ref α
    → Expr ref β
    → Expr ref γ
  Op3 -- A ternary operator.
    ∷ (Streamable α, Streamable β, Streamable γ, Streamable δ)
    ⇒ Op3 α β γ δ
    → Expr ref α
    → Expr ref β
    → Expr ref γ
    → Expr ref δ

deriving instance Show (Expr ref α)

data Op1 ∷ * → * → * where
  -- Boolean
  Not     ∷ Op1 Bool Bool
  -- Numeric
  Abs     ∷ Num α ⇒ Op1 α α
  Signum  ∷ Num α ⇒ Op1 α α
  -- Fractional
  Recip   ∷ Fractional α ⇒ Op1 α α
  -- Floating
  Exp     ∷ Floating α ⇒ Op1 α α
  Sqrt    ∷ Floating α ⇒ Op1 α α
  Log     ∷ Floating α ⇒ Op1 α α
  Sin     ∷ Floating α ⇒ Op1 α α
  Tan     ∷ Floating α ⇒ Op1 α α
  Cos     ∷ Floating α ⇒ Op1 α α
  Asin    ∷ Floating α ⇒ Op1 α α
  Atan    ∷ Floating α ⇒ Op1 α α
  Acos    ∷ Floating α ⇒ Op1 α α
  Sinh    ∷ Floating α ⇒ Op1 α α
  Tanh    ∷ Floating α ⇒ Op1 α α
  Cosh    ∷ Floating α ⇒ Op1 α α
  Asinh   ∷ Floating α ⇒ Op1 α α
  Atanh   ∷ Floating α ⇒ Op1 α α
  Acosh   ∷ Floating α ⇒ Op1 α α

deriving instance Eq (Op1 α β)

deriving instance Show (Op1 α β)

data Op2 ∷ * → * → * → * where
  -- Boolean
  (:||:)  ∷ Op2 Bool Bool Bool
  (:&&:)  ∷ Op2 Bool Bool Bool
  -- Numeric
  (:+:)   ∷ Num α ⇒ Op2 α α α
  (:-:)   ∷ Num α ⇒ Op2 α α α
  (:*:)   ∷ Num α ⇒ Op2 α α α
  -- Fractional
  (:/:)   ∷ Fractional α ⇒ Op2 α α α
  -- Integral
  Mod     ∷ Integral α ⇒ Op2 α α α
  -- Equality
  (:==:)  ∷ Eq α  ⇒ Op2 α α Bool
  (:/=:)  ∷ Eq α  ⇒ Op2 α α Bool
  -- Relational
  (:<:)   ∷ Ord α ⇒ Op2 α α Bool
  (:>:)   ∷ Ord α ⇒ Op2 α α Bool
  (:<=:)  ∷ Ord α ⇒ Op2 α α Bool
  (:>=:)  ∷ Ord α ⇒ Op2 α α Bool
  -- Floating
  Pow     ∷ Floating α ⇒ Op2 α α α
  LogBase ∷ Floating α ⇒ Op2 α α α

deriving instance Eq (Op2 α β γ)

deriving instance Show (Op2 α β γ)

data Op3 ∷ * → * → * → * → * where
  -- Mutex (a.k.a. if-then-else)
  Mux ∷ Op3 Bool α α α

deriving instance Eq (Op3 α β γ δ)

deriving instance Show (Op3 α β γ δ)
