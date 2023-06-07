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
  t  <- newHttpTransport
  let c = parserPrettyCodec

  let producer :: IO ()
      producer = tStart t port eq

      consumer :: Config -> IO ()
      consumer cfg = do
        event <- eqDequeue eq
        cfg' <- handleEvent cfg t c event
        consumer cfg'

  bracket (forkIO producer) killThread $ \_pid ->
    consumer cfg0

handleEvent :: Config -> Transport -> Codec -> Event -> IO Config
handleEvent cfg t c ev = case ev of
  InputEv from bs -> case cDecode c bs of
    Left err -> do
      putStrLn ("Decode error: " ++ err)
      tRespond t from ("Decode error: " <> fromString err)
      return cfg
    Right input -> do
      let (cfg', output) = stepSM "0" input cfg -- XXX: hardcoded sm id
      tRespond t from (cEncode c output)
      return cfg'
  QuitEv -> exitSuccess
