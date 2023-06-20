module Smarrow.AST.Operations where

data UnaryOp = Not | Negate
  deriving (Eq, Show)

data BinOp = Add | Mult | And | Or | Eq | Lt
  deriving (Eq, Show, Read)
