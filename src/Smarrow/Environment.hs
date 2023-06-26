{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Smarrow.Environment where

import Control.Applicative ((<|>))
import Data.List (findIndex)

import Smarrow.AST

------------------------------------------------------------------------

-- XXX: Move? Only used here.

data TypeDecl = TypeDecl TypeName TypeSig
  deriving (Eq, Show)

boolTypeDecl :: TypeDecl
boolTypeDecl = TypeDecl "Bool" (TypeSig [ Constructor "True" []
                                        , Constructor "False" []])

data Constructor = Constructor
  { cConName    :: ConName
  , cParameters :: [Parameter]
  }
  deriving (Eq, Show)

data Parameter = Parameter
  { pFieldName :: FieldName
  , pType      :: Type
  }
  deriving (Eq, Show)

newtype TypeSig = TypeSig [Constructor]
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

extendEnvLang :: Env -> [(Type, Type)] -> Env
extendEnvLang env tys =
  env { envTypeDecls = TypeDecl "Input" (TypeSig cons) : envTypeDecls env }
  where
    cons = map (go . fst) tys

    go (Defined (TypeName n)) = Constructor (ConName n) []
    go _ = error "extendEnvLang"

extendEnvRecord :: Env -> [Parameter] -> Env
extendEnvRecord env params =
  env { envTypeDecls = TypeDecl "_" (TypeSig [Constructor "_" params]) : envTypeDecls env }

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
    go (TypeDecl _ty (TypeSig cons) : typeDecls) =
      findIndex (== conName) conNames <|> go typeDecls
      where
        conNames = map cConName cons

-- XXX: would be nice if we had the type here?
fieldNameIndex :: Env -> FieldName -> Int
fieldNameIndex env fieldName = case go (envTypeDecls env) of
  Nothing -> error ("The field name: " ++ show fieldName ++
                    " isn't in type declarations: " ++ show (envTypeDecls env))
  Just ix -> ix
  where
    go [] = Nothing
    go (TypeDecl _ty (TypeSig cons) : typeDecls) =
      go' (map cParameters cons) <|> go typeDecls
      where
        go' [] = Nothing
        go' (p : ps) = findIndex (== fieldName) (map pFieldName p) <|> go' ps
