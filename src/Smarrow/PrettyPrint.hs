module Smarrow.PrettyPrint where

import qualified Data.ByteString.Char8 as BS8

import Smarrow.Value
import Smarrow.Syntax

------------------------------------------------------------------------

prettyValue :: Value -> String
prettyValue UnitV           = "()"
prettyValue (LitV (Int i))  = show i
prettyValue (LitV (Char c)) = show c
prettyValue (PairV l r)     = "(" ++ prettyValue l ++ ", " ++ prettyValue r ++ ")"
prettyValue (ConV conName)  = BS8.unpack (unConName conName)
prettyValue v               = error ("prettyValue: " ++ show v)
