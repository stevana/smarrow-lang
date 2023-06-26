module Smarrow.Interpreter where

import Control.Monad.Trans.State.Strict (State, get, put, runState)

import Smarrow.AST
import Smarrow.Environment
import Smarrow.Translate

------------------------------------------------------------------------

eval :: CCC -> Value -> State Value Value
eval Fst           (PairV l _r)     = return l
eval Snd           (PairV _l r)     = return r
eval (f :&&& g)    x                = PairV <$> eval f x <*> eval g x
eval (f :>>> g)    x                = eval f x >>= eval g
eval (f :<<< g)    x                = eval f   =<< eval g x
eval Id            x                = return x
-- eval App           (PairV (FunV f) (PairV x y)) = return (evalBinOp f x y)
eval (LitA l)      _x               = return (evalLit l)
eval (f :||| _g)   (LeftV x)        = eval f x
eval (_f :||| g)   (RightV y)       = eval g y
eval (f :||| _g)   (Inject _c 0 x)  = eval f x
eval (_f :||| g)   (Inject _c 1 y)  = eval g y
-- XXX: Generalise (:***)
-- eval (Parallel fs)   (Product xs)     = Product <$> zipWithM eval fs xs
eval (FanOut fs)   x                = Product <$> mapM (\f -> eval f x) fs
eval (FanIn fs)     (Inject _c i x) = eval (fs !! i) x
eval (Project _f i) (Product xs)    = return (xs !! i)
eval Dup           v                = return (PairV v v)
-- XXX: Generalise First, Second?
eval (First f)     (PairV x y)      = PairV <$> eval f x <*> pure y
eval (Second g)    (PairV x y)      = PairV x <$> eval g y
eval (BinOpA op)   (PairV x y)      = return (evalBinOp op x y)
eval (InjectA c i) v                = return (ConV c)
eval Get           UnitV            = get
eval Put           s'               = put s' >> return UnitV
eval Unit          _v               = return UnitV
eval c v = error (show (c, v))

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp Eq x y | x == y    = TrueV
                 | otherwise = FalseV
evalBinOp And  TrueV     b        = b
evalBinOp Or   TrueV     _b       = TrueV
evalBinOp Or   FalseV    b        = b
evalBinOp Add  (IntV i)  (IntV j) = IntV (i + j)
evalBinOp Mult (IntV i)  (IntV j) = IntV (i * j)
evalBinOp And  _x _y = error "evalBinOp, impossible due to typechecking"
evalBinOp Or   _x _y = error "evalBinOp, impossible due to typechecking"
evalBinOp Add  _x _y = error "evalBinOp, impossible due to typechecking"
evalBinOp Mult _x _y = error "evalBinOp, impossible due to typechecking"

evalLit :: Lit -> Value
evalLit (Int i)  = IntV i
evalLit (Char c) = CharV c

------------------------------------------------------------------------

interpret :: Env -> Expr -> (Value -> Value -> (Value, Value))
interpret env expr input state = (state', output)
  where
    input_ = translateValueCons env input
    -- state_ = translateValueCons env state
    (output, state') = runState (eval (translate env expr) input_) state

run :: Env -> CCC -> Value -> Value -> Value
run env code input state = PairV state' output
  where
    input_ = translateValueCons env input
    (output, state') = runState (eval code input_) state

------------------------------------------------------------------------

-- Possible optimisations:
-- https://stefan-marr.de/2023/06/squeezing-a-little-more-performance-out-of-bytecode-interpreters/
