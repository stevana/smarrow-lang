{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Smarrow.AST.Surface where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.String (IsString)

import Smarrow.AST.Literals
import Smarrow.AST.Names
import Smarrow.AST.Operations

------------------------------------------------------------------------

data Type
  = Defined TypeName
  | UnitT
  | PairT Type Type
  | RecordT (Record Expr)
  -- | AnonymousSum [ConName] -- XXX: Only enum atm, add products of types.
  deriving (Eq, Show, Read)

type Record a = [(FieldName, Maybe Type, Maybe a)]
  -- deriving (Eq, Show, Read)

data Expr
  = LitE Lit
  | VarE Var
  | UnaryOp UnaryOp Expr
  | BinOp BinOp Expr Expr
  | Proc Pat Cmd
  | ReturnE
  | IdE
  | ComposeE Expr Expr
  | LeftE Expr
  | RightE Expr
  | ChoiceE Expr Expr
  | UnitE
  | PairE Expr Expr
  | FstE Expr
  | SndE Expr
  | FanOutE Expr Expr
  | Con ConName -- [Expr]
  | GetE
  | PutE
  | RecordE (Record Expr)
  | ProjectE Expr FieldName
  | UpdateE Expr Expr
  deriving (Eq, Show, Read)

data Cmd
  = Expr :-< Expr
  -- | Kappa [Pat] Cmd
  -- | Op Expr [Cmd]
  -- XXX: | InfixOp Cmd BinOp Cmd
  -- XXX: | Let [Decl] Cmd
  -- | LetCmd (VarDelc Cmd) Cmd -- ?
  | If Expr Cmd Cmd
  | Case Expr [Alt]
  -- | Paren Cmd
  | Do [Stmt] Cmd
  -- | App Cmd Expr
  -- | CmdVar Name
  deriving (Eq, Show, Read)

infix 9 :-<
infix 8 :<-

data Stmt = Pat :<- Cmd
  deriving (Eq, Show, Read)

data Pat = WildP | UnitP | TupleP Pat Pat | ConNameP ConName | VarP Var
  deriving (Show, Read, Eq)

data Alt = Alt Pat GuardedAlts -- [Decl]
  deriving (Eq, Show, Read)

data GuardedAlts
  = UnguardedAlt Cmd
  | GuardedAlts [GuardedAlt]
  deriving (Eq, Show, Read)

data GuardedAlt = GuardedAlt Expr Cmd
  deriving (Eq, Show, Read)
