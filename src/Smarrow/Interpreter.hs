module Smarrow.Interpreter where

import Smarrow.CCC
import Smarrow.Syntax
import Smarrow.Translate
import Smarrow.Value

------------------------------------------------------------------------

eval :: CCC -> Value -> Value
eval Fst        (PairV l _r)  = l
eval Snd        (PairV _l r)  = r
eval (f :&&& g) x             = PairV (eval f x) (eval g x)
eval (f :>>> g) x             = eval g (eval f x)
eval (f :<<< g) x             = eval f (eval g x)
eval Id         x             = x
eval App        (PairV (FunV f) (PairV x y)) = evalBinOp f x y
eval (ConstA c) _x            = evalConst c
eval (f :||| _g) (LeftV x)    = eval f x
eval (_f :||| g) (RightV y)   = eval g y
eval (f :||| _g) (Inject 0 x) = eval f x
eval (_f :||| g) (Inject 1 y) = eval g y
eval (FanOut fs) (Product xs) = Product (zipWith eval fs xs)
eval (FanIn fs)  (Inject i x) = eval (fs !! i) x
eval (Project i) (Product xs) = xs !! i
eval Dup         v            = PairV v v
eval (First f)   (PairV x y)  = PairV (eval f x) y
eval (Second g)  (PairV x y)  = PairV x (eval g y)
eval (BinOpA op) (PairV x y)  = evalBinOp op x y
eval c v = error (show (c, v))

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp Eq x y | x == y    = trueV
                 | otherwise = falseV
-- evalBinOp And  (BoolV a) (BoolV b) = BoolV (a && b) -- XXX: should bools be built-in or not?
-- evalBinOp Or   (BoolV a) (BoolV b) = BoolV (a || b)
evalBinOp Add  (IntV i)  (IntV j)  = IntV (i + j)
evalBinOp Mult (IntV i)  (IntV j)  = IntV (i * j)
evalBinOp And  _x _y = error "evalBinOp, impossible due to typechecking"
evalBinOp Or   _x _y = error "evalBinOp, impossible due to typechecking"
evalBinOp Add  _x _y = error "evalBinOp, impossible due to typechecking"
evalBinOp Mult _x _y = error "evalBinOp, impossible due to typechecking"

evalConst :: Const -> Value
evalConst (Bool b) = if b then trueV else falseV
evalConst (Int  i) = IntV i

interpret :: Expr -> (Value -> Value)
interpret = eval . translate
