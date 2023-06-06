{-# LANGUAGE OverloadedStrings #-}

module Test where

import Test.Tasty.HUnit

import Smarrow.Environment
import Smarrow.Interpreter
import Smarrow.Parser
import Smarrow.Translate
import Smarrow.Value

------------------------------------------------------------------------

(==>) :: (String, String) -> String -> Assertion
(fun, arg) ==> expected =
  case testParser pSrc' fun of
    Left  err     -> fail err
    Right funExpr -> case testParser pValue' arg of
      Left err'    -> fail err'
      Right argVal -> case testParser pValue' expected of
        Left err''        -> fail err''
        Right expectedVal ->
          snd (interpret funExpr (translateValueCons defaultEnv argVal) UnitV) @?= expectedVal

(~~>) :: ((String, String), String) -> String -> Assertion
((fun, arg), state) ~~> expected =
  case testParser pSrc' fun of
    Left  err     -> fail err
    Right funExpr -> case testParser pValue' arg of
      Left err'    -> fail err'
      Right argVal -> case testParser pValue' state of
        Left err'' -> fail err''
        Right stateVal -> case testParser pValue' expected of
          Left err'''       -> fail err'''
          Right expectedVal ->
            uncurry PairV (interpret funExpr (translateValueCons defaultEnv argVal) (translateValueCons defaultEnv stateVal))
              @?= expectedVal

(@) :: a -> b -> (a, b)
(@) = (,)

------------------------------------------------------------------------

unit_swap :: Assertion
unit_swap = "proc (x, y) -> return -< (y, x)" @ "(1, 2)" ==> "(2, 1)"

unit_idId :: Assertion
unit_idId = "proc x -> do { y <- return -< x; return -< y }" @ "1" ==> "1"

unit_dup :: Assertion
unit_dup = "proc x -> return -< (x, x)" @ "1" ==> "(1, 1)"

unit_productExpr :: Assertion
unit_productExpr =
  "proc x -> do { y <- return -< (x, x); return -< (y, x) }" @ "1" ==> "((1, 1), 1)"

unit_productPattern :: Assertion
unit_productPattern =
  "proc x -> do { (y, _) <- return -< x; (z, _) <- return -< y; return -< z }" @ "((1, 2), 3)" ==> "1"

unit_assoc :: Assertion
unit_assoc = "proc (x, (y, z)) -> return -< ((x, y), z)" @ "(1, (2, 3))" ==> "((1, 2), 3)"

unit_ifTrue :: Assertion
unit_ifTrue = "proc b -> if b then return -< 1 else return -< 2" @ "True" ==> "1"

unit_caseNot :: Assertion
unit_caseNot = "proc b -> case b of { False -> return -< True; True -> return -< False }" @ "True" ==> "False"

unit_ifEq2 :: Assertion
unit_ifEq2 = "proc i -> if i == 2 then return -< True else return -< False" @ "2" ==> "True"

------------------------------------------------------------------------

unit_incrState :: Assertion
unit_incrState = "proc () -> do { i <- get -< (); j <- put -< i + 1; return -< j }" @ "()" @ "0" ~~> "(1, ())"
