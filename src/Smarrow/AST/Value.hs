{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

module Smarrow.AST.Value where

import Smarrow.AST.Literals
import Smarrow.AST.Names
import Smarrow.AST.Operations
import Smarrow.AST.Surface

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
  | RecordV (Record Value)
  deriving (Eq, Show, Read)

pattern IntV :: Int -> Value
pattern IntV i = LitV (Int i)

pattern CharV :: Char -> Value
pattern CharV i = LitV (Char i)

pattern TrueV :: Value
pattern TrueV = Inject "True" 0 UnitV

pattern FalseV :: Value
pattern FalseV = Inject "False" 1 UnitV
