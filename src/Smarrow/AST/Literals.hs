module Smarrow.AST.Literals where

data Lit
  = Int Int
  | Char Char
  deriving (Eq, Show, Read)
