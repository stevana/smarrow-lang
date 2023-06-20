{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smarrow.AST.Names where

import Data.String (IsString)
import Data.ByteString (ByteString)

------------------------------------------------------------------------

newtype ConName = ConName ByteString
  deriving (Eq, Show, Read, IsString)

unConName :: ConName -> ByteString
unConName (ConName bs) = bs

newtype Var = Var ByteString
  deriving (Show, Eq, Ord, IsString)

newtype TypeName = TypeName ByteString
  deriving (Eq, Ord, Show, Read, IsString)
