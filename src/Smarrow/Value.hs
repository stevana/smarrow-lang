{-# LANGUAGE StrictData #-}

module Smarrow.Value where

import Smarrow.Syntax

------------------------------------------------------------------------

data Value
  = PairV Value Value
  | IntV Int
  | LeftV Value
  | RightV Value
  | FunV BinOp
  | UnitV
  | Product [Value]
  | Inject Int Value
  | ConV ConName
  deriving (Eq, Show)

trueV :: Value
trueV = LeftV UnitV

falseV :: Value
falseV = RightV UnitV
