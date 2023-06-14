{-# LANGUAGE OverloadedStrings #-}

module DeployTest where

import Test.Tasty.HUnit

import Smarrow.CCC
import Smarrow.Deploy
import Smarrow.Parser
import Smarrow.Translate
import Smarrow.Value

------------------------------------------------------------------------

-- XXX: Proper constructors for Incr and Read.
counterCode :: String
counterCode = unlines
  ["proc i -> case i of"
  ,   "{ True  -> do { i <- get -< (); put -< i + 1 }"
  ,   "; False -> get -< ()"
  ,   "}"
  ]

counterCode2 :: String
counterCode2 = unlines
  ["proc i -> case i of"
  ,   "{ True  -> do { i <- get -< (); put -< i + 2 }"
  ,   "; False -> get -< ()"
  ,   "}"
  ]

unit_deployCounter :: Assertion
unit_deployCounter = withEventLoop port $ do
  c <- newTestClient port
  case (testParser pSrc' counterCode, testParser pSrc' counterCode2) of
    (Left err, _)  -> fail err
    (_,  Left err) -> fail err
    (Right counterExpr, Right counterExpr2) -> do
      let smid     = "counter"
          counter  = translate counterExpr
          counter2 = translate counterExpr2
      spawn c smid counter (IntV 0)
      _resp <- call_ c smid (ConV "True")
      resp  <- call_ c smid (ConV "False")
      resp @?= IntV 1
      upgrade c smid counter counter2 Id
      _resp  <- call_ c smid (ConV "True")
      resp2  <- call_ c smid (ConV "False")
      resp2 @?= IntV 3
  where
    port = 8000
