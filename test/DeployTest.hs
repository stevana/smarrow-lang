{-# LANGUAGE OverloadedStrings #-}

module DeployTest where

import Test.Tasty.HUnit

import Smarrow.Deploy
import Smarrow.Value
import Smarrow.Parser
import Smarrow.Translate

------------------------------------------------------------------------

counterCode :: String
counterCode = unlines
  ["proc i -> case i of"
  ,   "{ True  -> do { i <- get -< (); put -< i + 1 }"
  ,   "; False -> get -< ()"
  ,   "}"
  ]

unit_deployCounter :: Assertion
unit_deployCounter = withEventLoop port $ do
  c <- newTestClient port
  case testParser pSrc' counterCode of
    Left err -> fail err
    Right counterExpr -> do
      let counter = translate counterExpr
      spawn c "counter" counter (IntV 0)
      _resp <- call_ c "counter" (ConV "True")
      resp  <- call_ c "counter" (ConV "False")
      resp @?= IntV 1
  where
    port = 8000
