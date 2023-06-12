{-# LANGUAGE OverloadedStrings #-}

module Smarrow.Deploy.EventLoop where

import Data.String (fromString)
import Control.Concurrent
import Control.Exception
import System.Exit

import Smarrow.Deploy.Event
import Smarrow.Deploy.Codec
import Smarrow.Deploy.Config
import Smarrow.Deploy.EventQueue
import Smarrow.Deploy.Transport

------------------------------------------------------------------------

eventLoop :: Int -> IO ()
eventLoop port = do
  let cfg0 = emptyConfig
  eq <- newStmEventQueue
  t  <- newHttpTransport port
  let c = readShowCodec

  let producer :: IO ()
      producer = tStart t eq

      consumer :: Config -> IO ()
      consumer cfg = do
        event <- eqDequeue eq
        cfg' <- handleEvent cfg t c event
        consumer cfg'

  bracket (forkIO producer) killThread $ \_pid ->
    consumer cfg0

handleEvent :: Config -> Transport -> Codec -> Event -> IO Config
handleEvent cfg t c ev = case ev of
  InputEv cid bs -> case cDecodeInput c bs of
    Left err -> do
      putStrLn ("Decode error: " ++ err)
      -- XXX: perhaps this shouldn't return ok200? Tag the response?
      tRespond t cid ("Decode error: " <> fromString err)
      return cfg
    Right (Input smId input) -> do
      let (cfg', output) = stepSM smId input cfg
      tRespond t cid (cEncodeOutput c output)
      return cfg'
  SpawnEv cid bs -> case cDecodeSpawn c bs of
    Left err -> undefined
    Right (Spawn smId code initState) -> do
      tRespond t cid "spawned"
      return (spawnSM smId code initState cfg)
  UpgradeEv {} -> undefined
  QuitEv -> exitSuccess
