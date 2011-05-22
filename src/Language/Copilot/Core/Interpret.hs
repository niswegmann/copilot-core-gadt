-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Core.Interpret
  ( interpret
  ) where

import Control.DeepSeq (NFData, deepseq)
import Language.Copilot.Core
import Language.Copilot.Core.HeteroMap (HeteroMap, Key)
import qualified Language.Copilot.Core.HeteroMap as H

-- | Interprets a CoPilot-specification.
interpret ∷ WithSpec α → [α]
interpret (WithSpec runSpec) =
  runSpec $ \ (Spec m n0) →
    evalExpr (continue m) n0

continue
  ∷ (HasType α, HeteroMap map)
   ⇒ map (Strm (Key map))
   → Key map α
   → [α]
continue m k = H.lookup k env
  where
    env = H.mapWithKey (const $ evalStrm $ continue m) m

evalStrm
  ∷ (∀ β . HasType β ⇒ key β → [β])
   → Strm key α
   → [α]
evalStrm c a0 = case a0 of
  Strm _ xs n → strict $ xs ++ evalExpr c n

-- A continuation-style evaluator:
evalExpr
  -- A continuation for evaluating the children of the node:
  ∷ (∀ β . HasType β ⇒ f β → [β])
   → Expr f α -- The node to be evaluated.
   → [α]      -- The resulting stream.
evalExpr c n0 = case n0 of
  Const x         → x `deepseq` repeat x
  Drop i k        → strict $ drop i (c k)
  Extern _        → error "eval: Extern"
  Op1 f k         → strict $ fmap     (evalOp1 f) (evalExpr c k)
  Op2 f k1 k2     → strict $ zipWith  (evalOp2 f) (evalExpr c k1)
                                                  (evalExpr c k2)
  Op3 f k1 k2 k3  → strict $ zipWith3 (evalOp3 f) (evalExpr c k1)
                                                  (evalExpr c k2)
                                                  (evalExpr c k3)

strict ∷ NFData α ⇒ [α] → [α]
strict (x:xs) = x `deepseq` (x : strict xs)
strict []     = []

evalOp1 ∷ Op1 α β → α → β
evalOp1 Not     = not
evalOp1 Abs     = abs
evalOp1 Signum  = signum
evalOp1 Recip   = recip
evalOp1 Exp     = exp
evalOp1 Sqrt    = sqrt
evalOp1 Log     = log
evalOp1 Sin     = sin
evalOp1 Tan     = tan
evalOp1 Cos     = cos
evalOp1 Asin    = asin
evalOp1 Atan    = atan
evalOp1 Acos    = acos
evalOp1 Sinh    = sinh
evalOp1 Tanh    = tanh
evalOp1 Cosh    = cosh
evalOp1 Asinh   = asinh
evalOp1 Atanh   = atanh
evalOp1 Acosh   = acosh

evalOp2 ∷ Op2 α β γ → α → β → γ
evalOp2 (:&&:)  = (&&)
evalOp2 (:||:)  = (||)
evalOp2 (:+:)   = (+)
evalOp2 (:-:)   = (-)
evalOp2 (:*:)   = (*)
evalOp2 (:/:)   = (/)
evalOp2 Mod     = mod
evalOp2 (:==:)  = (==)
evalOp2 (:/=:)  = (/=)
evalOp2 (:<:)   = (<)
evalOp2 (:>:)   = (>)
evalOp2 (:<=:)  = (<=)
evalOp2 (:>=:)  = (>=)
evalOp2 Pow     = (**)
evalOp2 LogBase = logBase

evalOp3 ∷ Op3 α β γ δ → α → β → γ → δ
evalOp3 Mux     = \ v x y → if v then x else y
