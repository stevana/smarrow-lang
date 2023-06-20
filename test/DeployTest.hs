{-# LANGUAGE OverloadedStrings #-}

module DeployTest where

import Test.Tasty.HUnit

import Smarrow.AST
import Smarrow.Deploy
import Smarrow.Environment
import Smarrow.Parser
import Smarrow.Translate

------------------------------------------------------------------------

-- XXX: Proper constructors for Incr and Read.
counterCode :: String
counterCode = unlines
  ["function i -> case i of"
  ,   "{ True  -> do { i <- get -< (); put -< i + 1 }"
  ,   "; False -> get -< ()"
  ,   "}"
  ]

counterCode2 :: String
counterCode2 = unlines
  ["function i -> case i of"
  ,   "{ True  -> do { i <- get -< (); put -< i + 2 }"
  ,   "; False -> get -< ()"
  ,   "}"
  ]

unit_deployCounter :: Assertion
unit_deployCounter = withEventLoop port $ do
  c <- newTestClient port
  case (testParser pExpr' counterCode, testParser pExpr' counterCode2) of
    (Left err, _)  -> fail err
    (_,  Left err) -> fail err
    (Right counterExpr, Right counterExpr2) -> do
      let smid     = "counter"
          counter  = translate defaultEnv counterExpr
          counter2 = translate defaultEnv counterExpr2
      spawn c smid counter (IntV 0) (LangDecl [])
      _resp <- call_ c smid (ConV "True")
      resp  <- call_ c smid (ConV "False")
      resp @?= IntV 1
      upgrade c smid counter counter2 Id (LangDecl [])
      _resp  <- call_ c smid (ConV "True")
      resp2  <- call_ c smid (ConV "False")
      resp2 @?= IntV 3
  where
    port = 8000
