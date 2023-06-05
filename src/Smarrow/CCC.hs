{-# LANGUAGE StrictData #-}

module Smarrow.CCC where

import Smarrow.Syntax (Const, BinOp)

------------------------------------------------------------------------

data CCC
  = Fst
  | Snd
  | CCC :&&& CCC
  | CCC :>>> CCC
  | CCC :<<< CCC
  | Id
  | App
  | CCC :||| CCC
  | ConstA Const
  | First CCC
  | Second CCC
  | Dup
  | CCC :*** CCC

  | Project Int
  | FanOut [CCC]
  | FanIn [CCC]

  | BinOpA BinOp
  deriving (Eq, Show)

simplify1 :: CCC -> CCC
simplify1 (Id :&&& Id) = Dup
simplify1 (a :&&& b)   = simplify1 a :&&& simplify1 b
simplify1 (a :>>> Id)  = simplify1 a
simplify1 (a :>>> b)   = simplify1 a :>>> simplify1 b
simplify1 a = a

simplify :: CCC -> CCC
simplify a | a' == a   = a
           | otherwise = simplify a'
  where
    a' = simplify1 a
