{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

module Smarrow.Value where

import Smarrow.Syntax

------------------------------------------------------------------------

data Value
  = PairV Value Value
  | LitV Lit
  | LeftV Value
  | RightV Value
  | FunV BinOp
  | UnitV
  | Product [Value]
  | Inject ConName Int Value
  | ConV ConName
  deriving (Eq, Show)

pattern IntV :: Int -> Value
pattern IntV i = LitV (Int i)

pattern CharV :: Char -> Value
pattern CharV i = LitV (Char i)

pattern TrueV :: Value
pattern TrueV = Inject "True" 0 UnitV

pattern FalseV :: Value
pattern FalseV = Inject "False" 1 UnitV
