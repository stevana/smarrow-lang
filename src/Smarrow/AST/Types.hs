{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smarrow.AST.Types where

import Data.ByteString (ByteString)
import Data.String (IsString)

import Smarrow.AST.Names

------------------------------------------------------------------------

data Type
  = Defined TypeName
  | AnonymousRecord [(FieldName, Type)]
  | AnonymousSum [ConName] -- XXX: Only enum atm, add products of types.
  deriving (Show, Read)

newtype FieldName = FieldName ByteString
  deriving (Eq, Ord, Show, Read, IsString)
