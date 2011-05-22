-- © 2011 National Institute of Aerospace / Galois, Inc.

-- |

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Core.HeteroMap
  ( HeteroMap (..)
  , Key2Int (..)
  ) where

import Control.Applicative (Applicative)
import Data.Monoid (Monoid)
import Language.Copilot.Core.Type (HasType)

class Key2Int key where
  key2int ∷ key a → Int

class Key2Int (Key map) ⇒ HeteroMap map where

  type Key map ∷ * → *

  lookup
    ∷ HasType a
     ⇒ Key map a
     → map f
     → f a

  mapWithKey
    ∷ (∀ a . Key map a → f a → g a)
     → map f
     → map g

  foldMapWithKey
    ∷ Monoid m
     ⇒ (∀ a . Key map a → f a → m)
     → map f
     → m

  foldMapWithKeyM
    ∷ (Monoid m, Applicative t)
     ⇒ (∀ a . Key map a → f a → t m)
     → map f
     → t m

  traverseWithKey
    ∷ Applicative t
     ⇒ (∀ a . Key map a → f a → t (g a))
     → map f
     → t (map g)
