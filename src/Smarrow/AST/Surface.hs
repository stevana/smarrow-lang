{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Smarrow.AST.Surface where

import Data.ByteString (ByteString)
import Data.String (IsString)

import Smarrow.AST.Literals
import Smarrow.AST.Types
import Smarrow.AST.Names

------------------------------------------------------------------------

newtype MachineName = MachineName ByteString
  deriving (Eq, Ord, Show, IsString)

data Machine = Machine
  { machineName      :: MachineName
  , machineState     :: StateDecl
  , machineLanguage  :: LanguageDecl
  , machineFunction  :: Expr
  }

data LanguageDecl = LanguageDecl
  { ldTypes :: [(Type, Type)]
  }

data StateDecl = StateDecl
  { sdType :: Type
  -- , sdDefaultValue :: Value
  }

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
  deriving (Eq, Show)

data UnaryOp = Not | Negate
  deriving (Eq, Show)

data BinOp = Add | Mult | And | Or | Eq | Lt
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
  deriving (Eq, Show)

infix 9 :-<
infix 8 :<-

data Stmt = Pat :<- Cmd
  deriving (Eq, Show)

data Pat = WildP | UnitP | TupleP Pat Pat | ConNameP ConName | VarP Var
  deriving (Show, Eq)

data Alt = Alt Pat GuardedAlts -- [Decl]
  deriving (Eq, Show)

data GuardedAlts
  = UnguardedAlt Cmd
  | GuardedAlts [GuardedAlt]
  deriving (Eq, Show)

data GuardedAlt = GuardedAlt Expr Cmd
  deriving (Eq, Show)
