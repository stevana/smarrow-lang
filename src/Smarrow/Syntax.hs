{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Smarrow.Syntax where

import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text)

------------------------------------------------------------------------

data Module = Module
  { moduleName      :: ModuleName
  , moduleTypeDecls :: [TypeDecl]
  , moduleMachines  :: [Machine]
  }

data Machine = Machine
  { machineName   :: MachineName
  -- , machineInput  :: TypeName
  -- , machineState  :: TypeName
  -- , machineOutput :: TypeName
  , machineProc   :: Expr
  }

newtype ModuleName = ModuleName Text
  deriving (Eq, Ord, Show, IsString)

newtype TypeName = TypeName Text
  deriving (Eq, Ord, Show, IsString)

newtype MachineName = MachineName Text
  deriving (Eq, Ord, Show, IsString)

data Const = Int Int | Bool Bool
  deriving (Eq, Show)

data UnaryOp = Not | Negate
  deriving (Eq, Show)

data BinOp = Add | Mult | And | Or | Eq | Lt
  deriving (Eq, Show)

data Expr
  = ConstE Const
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
  deriving (Eq, Show)

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

newtype Var = Var ByteString
  deriving (Show, Eq, Ord, IsString)

data Pat = WildP | UnitP | TupleP Pat Pat | ConNameP ConName | VarP Var
  deriving (Show, Eq)

newtype DeclName = DeclName Text
  deriving (Show, Eq, IsString)

data Decl = Decl DeclName Expr
  deriving (Eq, Show)

data TypeDecl = TypeDecl TypeName TypeSig
  deriving (Eq, Show)

boolTypeDecl :: TypeDecl
boolTypeDecl = TypeDecl "Bool" (TypeSig ["True", "False"])

data TypeSig = TypeSig [ConName]
  deriving (Eq, Show)

newtype ConName = ConName ByteString
  deriving (Eq, Show, IsString)

data Alt = Alt Pat GuardedAlts [Decl]
  deriving (Eq, Show)

data GuardedAlts
  = UnguardedAlt Cmd
  | GuardedAlts [GuardedAlt]
  deriving (Eq, Show)

data GuardedAlt = GuardedAlt Expr Cmd
  deriving (Eq, Show)

------------------------------------------------------------------------

fvP :: Pat -> Set Var
fvP (VarP v)     = Set.singleton v
fvP (TupleP p q) = fvP p `Set.union` fvP q
fvP WildP        = Set.empty
fvP (ConNameP _) = Set.empty
fvP UnitP        = Set.empty

fvE :: Expr -> Set Var
fvE (Proc pat body) = fvC body `Set.difference` fvP pat
fvE (VarE v)        = Set.singleton v
fvE ReturnE         = Set.empty
fvE (PairE l r)     = fvE l `Set.union` fvE r
fvE IdE             = Set.empty
fvE e = error (show e)

fvC :: Cmd -> Set Var
fvC (f :-< a)        = fvE f `Set.union` fvE a
fvC (If b t f)       = fvE b `Set.union` fvC t `Set.union` fvC f
fvC (Case _e _as)    = undefined
fvC (Do _stmts _cmd) = undefined
