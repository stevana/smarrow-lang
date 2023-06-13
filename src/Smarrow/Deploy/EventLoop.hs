{-# LANGUAGE OverloadedStrings #-}

module Smarrow.Deploy.EventLoop where

import Control.Concurrent
import Control.Exception
import Data.ByteString (ByteString)
import Data.String (fromString)
import System.Exit

import Smarrow.Deploy.Codec
import Smarrow.Deploy.Config
import Smarrow.Deploy.Event
import Smarrow.Deploy.EventQueue
import Smarrow.Deploy.PendingRequests
import Smarrow.Deploy.Transport
import Smarrow.Environment
import Smarrow.Translate

------------------------------------------------------------------------

eventLoop :: Int -> MVar () -> IO ()
eventLoop port ready = do
  let cfg0 = emptyConfig
  eq <- newStmEventQueue
  t  <- newHttpTransport port
  let c = readShowCodec

  let producer :: IO ()
      producer = tStart t eq ready

      consumer :: Config -> IO ()
      consumer cfg = do
        event <- eqDequeue eq
        cfg' <- handleEvent cfg t c event
        consumer cfg'

  bracket (forkIO producer) killThread $ \_pid ->
    consumer cfg0

withEventLoop :: Int -> IO a -> IO a
withEventLoop port io = do
  ready <- newEmptyMVar
  bracket (forkIO (eventLoop port ready)) killThread $ \_pid -> do
    takeMVar ready
    io

handleEvent :: Config -> Transport -> Codec -> Event -> IO Config
handleEvent cfg t c ev = case ev of
  InputEv cid bs ->
    withSuccessDecode (cDecodeInput c) cid bs $ \(Input smid input) -> do
      let input' = translateValueCons defaultEnv input
          (cfg', output) = stepSM smid input' cfg
      tRespond t cid (cEncodeOutput c (Output output))
      return cfg'
  SpawnEv cid bs ->
    withSuccessDecode (cDecodeSpawn c) cid bs $ \(Spawn smid code initState) -> do
      tRespond t cid "spawned"
      return (spawnSM smid code initState cfg)
  UpgradeEv cid bs ->
    withSuccessDecode (cDecodeUpgrade c) cid bs $ \(Upgrade smid oldCode newCode stateMigration) ->
      return (upgradeSM smid oldCode newCode stateMigration cfg)
  QuitEv -> exitSuccess
  where
    withSuccessDecode :: (ByteString -> Either String a) -> ClientId -> ByteString
                      -> (a -> IO Config) -> IO Config
    withSuccessDecode decode cid bs k = do
      case decode bs of
        Left err -> do
          putStrLn ("Decode error: " ++ err)
          -- XXX: perhaps this shouldn't return ok200? Tag the response?
          tRespond t cid (fromString err)
          return cfg
        Right x -> k x
