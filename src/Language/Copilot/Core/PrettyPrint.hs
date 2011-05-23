{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Copilot.Core.PrettyPrint
  ( prettyPrint
  ) where

import Language.Copilot.Core
import Language.Copilot.Core.HeteroMap (HeteroMap, Key)
import qualified Language.Copilot.Core.HeteroMap as H
import Text.PrettyPrint.HughesPJ (Doc, ($$), (<+>), text, parens)
import qualified Text.PrettyPrint.HughesPJ as PP

prettyPrint ∷ WithSpec α → String
prettyPrint (WithSpec runSpec) = runSpec $ PP.render . ppSpec

data Name α = Name { unName ∷ String }

mkName ∷ Int → Name α
mkName = Name . ("s" ++) . show

ppName ∷ Name α → Doc
ppName = text . unName

ppSpec ∷ Spec map α → Doc
ppSpec (Spec m e) = cs $$ text "out =" <+> ppExpr nm e
  where
    nm = H.mapWithKey (\ k _ → mkName (H.key2Int k)) m
    cs = H.foldWithKey (ppStrmAcc nm) PP.empty m

ppStrmAcc
  ∷ HeteroMap map
  ⇒ map Name
  → Key map α
  → Strm (Key map) α
  → Doc
  → Doc
ppStrmAcc m k (Strm t xs e) acc =
  name <+> text ":" <+> typ <+> text "=" <+> buf <+> text "++" <+> expr $$ acc
  where
    name = ppName (H.lookup k m)
    typ  = text (show t)
    expr = ppExpr m e
    buf  = text (show xs)

ppExpr
  ∷ HeteroMap map
  ⇒ map Name
  → Expr (Key map) α
  → Doc
ppExpr m e0 =
  case e0 of
    Const x         → text (show x)
    Drop 0 k        → ppName (H.lookup k m)
    Drop i k        → text "drop" <+> PP.int i <+> ppName (H.lookup k m)
    Extern cs       → text "extern" <+> text "\"" <+> text cs <+> text "\""
    Op1 op e        → ppOp1 op (ppExpr m e)
    Op2 op e1 e2    → ppOp2 op (ppExpr m e1) (ppExpr m e2)
    Op3 op e1 e2 e3 → ppOp3 op (ppExpr m e1) (ppExpr m e2) (ppExpr m e3)

ppOp1 ∷ Op1 α β → Doc → Doc
ppOp1 op doc =
  case op of
    Not     -> text "not"    <+> doc
    Abs     -> text "abs"    <+> doc
    Signum  -> text "signum" <+> doc
    Recip   -> text "recip"  <+> doc
    Exp     -> text "exp"    <+> doc
    Sqrt    -> text "sqrt"   <+> doc
    Log     -> text "log"    <+> doc
    Sin     -> text "sin"    <+> doc
    Tan     -> text "tan"    <+> doc
    Cos     -> text "cos"    <+> doc
    Asin    -> text "asin"   <+> doc
    Atan    -> text "atan"   <+> doc
    Acos    -> text "acos"   <+> doc
    Sinh    -> text "sinh"   <+> doc
    Tanh    -> text "tanh"   <+> doc
    Cosh    -> text "cosh"   <+> doc
    Asinh   -> text "asinh"  <+> doc
    Atanh   -> text "atanh"  <+> doc
    Acosh   -> text "acosh"  <+> doc

ppOp2 ∷ Op2 α β γ → Doc → Doc → Doc
ppOp2 op doc1 doc2 =
  case op of
    -- Boolean
    (:||:)  -> parens $ doc1 <+> text "||" <+> doc2
    (:&&:)  -> parens $ doc1 <+> text "&&" <+> doc2
    (:+:)   -> parens $ doc1 <+> text "+"  <+> doc2
    (:-:)   -> parens $ doc1 <+> text "_"  <+> doc2
    (:*:)   -> parens $ doc1 <+> text "*"  <+> doc2
    (:/:)   -> parens $ doc1 <+> text "/"  <+> doc2
    (:==:)  -> parens $ doc1 <+> text "==" <+> doc2
    (:/=:)  -> parens $ doc1 <+> text "/=" <+> doc2
    (:<:)   -> parens $ doc1 <+> text "<"  <+> doc2
    (:>:)   -> parens $ doc1 <+> text ">"  <+> doc2
    (:<=:)  -> parens $ doc1 <+> text "<=" <+> doc2
    (:>=:)  -> parens $ doc1 <+> text ">=" <+> doc2
    Mod     -> text "mod"     <+> (parens doc1) <+> (parens doc1)
    Pow     -> text "pow"     <+> (parens doc1) <+> (parens doc1)
    LogBase -> text "logbase" <+> (parens doc1) <+> (parens doc1)

ppOp3 ∷ Op3 α β γ δ → Doc → Doc → Doc → Doc
ppOp3 op doc1 doc2 doc3 =
  case op of
    Mux     -> text "if"   <+> doc1 <+>
               text "then" <+> doc2 <+>
               text "else" <+> doc3
