-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

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
  key2Int ∷ key α → Int

class Key2Int (Key map) ⇒ HeteroMap map where

  type Key map ∷ * → *

  lookup
    ∷ HasType α
    ⇒ Key map α
    → map f
    → f α

  mapWithKey
    ∷ (∀ α . Key map α → f α → g α)
    → map f
    → map g

  fold
    ∷ (∀ α . f α → β → β)
    → β
    → map f
    → β

  foldWithKey
    ∷ (∀ α . Key map α → f α → β → β)
    → β
    → map f
    → β

  foldMapWithKey
    ∷ Monoid m
    ⇒ (∀ α . Key map α → f α → m)
    → map f
    → m

  foldMapWithKeyM
    ∷ (Monoid m, Applicative t)
    ⇒ (∀ α . Key map α → f α → t m)
    → map f
    → t m

  traverseWithKey
    ∷ Applicative t
    ⇒ (∀ α . Key map α → f α → t (g α))
    → map f
    → t (map g)
