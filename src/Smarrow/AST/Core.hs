{-# LANGUAGE StrictData #-}

module Smarrow.AST.Core where

import Smarrow.AST.Operations (BinOp)
import Smarrow.AST.Literals
import Smarrow.AST.Names
import Smarrow.AST.Types

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
  | Project FieldName Int
  | FanOut [CCC] -- Generalises :&&&
  | InjectA ConName Int
  | FanIn [CCC] -- Generalised :|||

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
