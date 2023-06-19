{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smarrow.AST.Types where

import Data.String (IsString)
import Data.Text (Text)

import Smarrow.AST.Names

------------------------------------------------------------------------

data Type
  = IntT
  | UnitT
  | AnonymousRecord [(FieldName, Type)]
  | AnonymousSum [ConName] -- XXX: Only enum atm, add products of types.

newtype FieldName = FieldName Text
  deriving (Eq, Ord, Show, IsString)
