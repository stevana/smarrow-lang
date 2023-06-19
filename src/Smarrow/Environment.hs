{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smarrow.Environment where

import Data.ByteString (ByteString)
import Data.String (IsString)
import Control.Applicative ((<|>))
import Data.List (findIndex)

import Smarrow.AST

------------------------------------------------------------------------

-- XXX: Move? Only used here.

newtype TypeName = TypeName ByteString
  deriving (Eq, Ord, Show, IsString)

data TypeDecl = TypeDecl TypeName TypeSig
  deriving (Eq, Show)

boolTypeDecl :: TypeDecl
boolTypeDecl = TypeDecl "Bool" (TypeSig ["True", "False"])

data TypeSig = TypeSig [ConName]
  deriving (Eq, Show)

------------------------------------------------------------------------

data Env = Env
  { envVars      :: [(Var, CCC)]
  , envTypeDecls :: [TypeDecl]
  }

emptyEnv :: Env
emptyEnv = Env [] []

defaultEnv :: Env
defaultEnv = emptyEnv { envTypeDecls = [boolTypeDecl] }

extendEnv :: Env -> Pat -> Env
extendEnv env pat = env { envVars = patVars pat ++
                                    map (\(v, p) -> (v, Snd :<<< p)) (envVars env) }
  where
    patVars (VarP v)     = [(v, Id)]
    patVars (TupleP p q) = map (fmap (Fst :>>>)) (patVars p) ++
                           map (fmap (Snd :>>>)) (patVars q)
    patVars WildP        = [("_", Id)]
    patVars (ConNameP _) = []
    patVars UnitP        = []

lookupVar :: Env -> Var -> CCC
lookupVar env v = -- debug "lookupVar" [("env", show (envVars env)), ("var", show v)] $
  case lookup v (envVars env) of
    Nothing   -> error ("lookupVar: no such var: " ++ show v)
    Just proj -> proj

conNameIndex :: Env -> ConName -> Int
conNameIndex env conName = case go (envTypeDecls env) of
  Nothing -> error ("The constructor name: " ++ show conName ++
                    " isn't in type declarations: " ++ show (envTypeDecls env))
  Just ix -> ix
  where
    go [] = Nothing
    go (TypeDecl _ty (TypeSig conNames) : typeDecls) =
      findIndex (== conName) conNames <|> go typeDecls
