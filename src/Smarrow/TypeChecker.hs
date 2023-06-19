{-# LANGUAGE StrictData #-}

module Smarrow.TypeChecker where

import Smarrow.AST

------------------------------------------------------------------------

data Typ = IntT | BoolT

data TypeError = TE

data Ctx = Ctx

check :: Ctx -> Expr -> Typ -> Either TypeError ()
check _ctx _expr _typ = return ()

infer :: Ctx -> Expr -> Either TypeError Typ
infer = undefined
