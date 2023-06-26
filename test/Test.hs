{-# LANGUAGE OverloadedStrings #-}

module Test where

import Test.Tasty.HUnit

import Smarrow.Environment
import Smarrow.Interpreter
import Smarrow.Parser
import Smarrow.AST

------------------------------------------------------------------------

(==>) :: (String, String) -> String -> Assertion
(fun, arg) ==> expected =
  case testParser pExpr' fun of
    Left  err     -> fail err
    Right funExpr -> case testParser pValue' arg of
      Left err'    -> fail err'
      Right argVal -> case testParser pValue' expected of
        Left err''        -> fail err''
        Right expectedVal ->
          snd (interpret defaultEnv funExpr argVal UnitV) @?= expectedVal

(~~>) :: ((String, String), String) -> String -> Assertion
((fun, arg), state) ~~> expected =
  case testParser pExpr' fun of
    Left  err     -> fail err
    Right funExpr -> case testParser pValue' arg of
      Left err'    -> fail err'
      Right argVal -> case testParser pValue' state of
        Left err'' -> fail err''
        Right stateVal -> case testParser pValue' expected of
          Left err'''       -> fail err'''
          Right expectedVal ->
            uncurry PairV (interpret defaultEnv funExpr argVal stateVal) @?= expectedVal

(@) :: a -> b -> (a, b)
(@) = (,)

------------------------------------------------------------------------

unit_swap :: Assertion
unit_swap = "function {x, y} -> return -< {y, x}" @ "{1, 2}" ==> "{2, 1}"

unit_idId :: Assertion
unit_idId = "function x -> do { y <- return -< x; return -< y }" @ "1" ==> "1"

unit_dup :: Assertion
unit_dup = "function x -> return -< {x, x}" @ "1" ==> "{1, 1}"

unit_productExpr :: Assertion
unit_productExpr =
  "function x -> do { y <- return -< {x, x}; return -< {y, x} }" @ "1" ==> "{{1, 1}, 1}"

unit_productPattern :: Assertion
unit_productPattern =
  "function x -> do { {y, _} <- return -< x; {z, _} <- return -< y; return -< z }" @ "{{1, 2}, 3}" ==> "1"

unit_assoc :: Assertion
unit_assoc = "function {x, {y, z}} -> return -< {{x, y}, z}" @ "{1, {2, 3}}" ==> "{{1, 2}, 3}"

unit_ifTrue :: Assertion
unit_ifTrue = "function b -> if b then return -< 1 else return -< 2" @ "True" ==> "1"

unit_caseNot :: Assertion
unit_caseNot = "function b -> case b of { False -> return -< True; True -> return -< False }" @ "True" ==> "False"

unit_ifEq2 :: Assertion
unit_ifEq2 = "function i -> if i == 2 then return -< True else return -< False" @ "2" ==> "True"

------------------------------------------------------------------------

unit_counterRead :: Assertion
unit_counterRead =
  "function i -> case i of { True  -> do { i <- get -< {}; put -< i + 1 }; False -> get -< {} }"
  @ "False"
  @ "0"
  ~~> "{0, 0}"

unit_counterIncr :: Assertion
unit_counterIncr =
  "function i -> case i of { True  -> do { i <- get -< {}; put -< i + 1 }; False -> get -< {} }"
  @ "True"
  @ "0"
  ~~> "{1, {}}"
