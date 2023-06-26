module Smarrow.PrettyPrint where

import qualified Data.ByteString.Char8 as BS8

import Smarrow.AST

------------------------------------------------------------------------

prettyValue :: Value -> String
prettyValue UnitV           = "{}"
prettyValue (LitV (Int i))  = show i
prettyValue (LitV (Char c)) = show c
prettyValue (PairV l r)     = "{" ++ prettyValue l ++ ", " ++ prettyValue r ++ "}"
prettyValue (ConV conName)  = BS8.unpack (unConName conName)
prettyValue (RecordV es0)   = "{" ++ prettyEntries es0 ++ "}"
  where
    prettyEntries [] = ""
    prettyEntries [e] = prettyEntry e
    prettyEntries (e : es) = prettyEntry e ++ ", " ++ prettyEntries es

    prettyEntry (FieldName field, mType, value) =
      BS8.unpack field ++
      maybe "" (\typ -> ":" ++ prettyType typ) mType ++
      " = " ++ prettyValue value
prettyValue v               = error ("prettyValue: " ++ show v)

prettyType :: Type -> String
prettyType (Defined (TypeName ty)) = BS8.unpack ty
prettyType ty = error ("prettyType: " ++ show ty)
