{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

module Smarrow.AST.Core where

import Smarrow.AST.Literals
import Smarrow.AST.Names
import Smarrow.AST.Operations (BinOp)

------------------------------------------------------------------------

data CCC
  = CCC :>>> CCC
  | CCC :<<< CCC
  | Id
  | App
  | LitA Lit
  | First CCC
  | Second CCC
  | Dup
  | CCC :*** CCC

  | Unit
  | Project FieldName Int
  | FanOut [CCC] -- Generalises :&&&
  | UpdateA

  | InjectA ConName Int
  | FanIn [CCC] -- Generalised :|||

  | BinOpA BinOp

  | Get
  | Put
  deriving (Eq, Show, Read)

pattern (:|||) :: CCC -> CCC -> CCC
pattern l :||| r = FanIn [l, r]

pattern (:&&&) :: CCC -> CCC -> CCC
pattern l :&&& r = FanOut [l, r]

pattern Fst :: CCC
pattern Fst = Project "fst" 0

pattern Snd :: CCC
pattern Snd = Project "snd" 1

simplify1 :: CCC -> CCC
simplify1 (Id :&&& Id) = Dup
simplify1 (a :&&& b)   = simplify1 a :&&& simplify1 b
simplify1 (a :||| b)   = simplify1 a :||| simplify1 b
simplify1 (a :>>> Id)  = simplify1 a
simplify1 (a :>>> b)   = simplify1 a :>>> simplify1 b
simplify1 a = a

simplify :: CCC -> CCC
simplify a | a' == a   = a
           | otherwise = simplify a'
  where
    a' = simplify1 a
