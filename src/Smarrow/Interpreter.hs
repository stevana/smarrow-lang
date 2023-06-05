module Smarrow.Interpreter where

import Smarrow.CCC
import Smarrow.Syntax
import Smarrow.Translate
import Smarrow.Value

------------------------------------------------------------------------

eval :: CCC -> Value -> Value
eval Fst         (PairV l _r)     = l
eval Snd         (PairV _l r)     = r
eval (f :&&& g)  x                = PairV (eval f x) (eval g x)
eval (f :>>> g)  x                = eval g (eval f x)
eval (f :<<< g)  x                = eval f (eval g x)
eval Id          x                = x
eval App         (PairV (FunV f) (PairV x y)) = evalBinOp f x y
eval (LitA l)    _x              = evalLit l
eval (f :||| _g) (LeftV x)       = eval f x
eval (_f :||| g) (RightV y)      = eval g y
eval (f :||| _g) (Inject _c 0 x) = eval f x
eval (_f :||| g) (Inject _c 1 y) = eval g y
eval (FanOut fs) (Product xs)    = Product (zipWith eval fs xs)
eval (FanIn fs)  (Inject _c i x) = eval (fs !! i) x
eval (Project i) (Product xs)    = xs !! i
eval Dup         v               = PairV v v
eval (First f)   (PairV x y)     = PairV (eval f x) y
eval (Second g)  (PairV x y)     = PairV x (eval g y)
eval (BinOpA op) (PairV x y)     = evalBinOp op x y
eval (InjectA c i) v             = ConV c
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

interpret :: Expr -> (Value -> Value)
interpret = eval . translate
