{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smarrow.AST.Names where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.String (IsString)

------------------------------------------------------------------------

newtype MachineName = MachineName ByteString
  deriving (Eq, Ord, Show, IsString)

machineNameString :: MachineName -> String
machineNameString (MachineName bs) = BS8.unpack bs

newtype ConName = ConName ByteString
  deriving (Eq, Show, Read, IsString)

unConName :: ConName -> ByteString
unConName (ConName bs) = bs

newtype Var = Var ByteString
  deriving (Show, Read, Eq, Ord, IsString)

newtype TypeName = TypeName ByteString
  deriving (Eq, Ord, Show, Read, IsString)

newtype FieldName = FieldName ByteString
  deriving (Eq, Ord, Show, Read, IsString)
