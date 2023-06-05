{-# LANGUAGE OverloadedStrings #-}

module Test where

import Test.Tasty.HUnit

import Smarrow.Interpreter
import Smarrow.Parser
import Smarrow.Translate
import Smarrow.Environment

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
          interpret funExpr (translateValueCons defaultEnv argVal) @?= expectedVal

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

unit_caseInt :: Assertion
unit_caseInt = "proc b -> case b of { False -> return -< 0; True -> return -< 1 }" @ "True" ==> "1"

unit_ifEq :: Assertion
unit_ifEq = "proc i -> if i == 2 then return -< 1 else return -< 0" @ "2" ==> "1"
