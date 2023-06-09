{-# LANGUAGE StrictData #-}

module Smarrow.CCC where

import Smarrow.Syntax (BinOp, ConName, Lit)

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
  | LitA Lit
  | First CCC
  | Second CCC
  | Dup
  | CCC :*** CCC

  | Unit
  | Project Int
  | FanOut [CCC]
  | InjectA ConName Int
  | FanIn [CCC]

  | BinOpA BinOp

  | Get
  | Put
  deriving (Eq, Show, Read)

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
