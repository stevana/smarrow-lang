{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Smarrow.Deploy.Transport where

import Control.Concurrent.MVar
import Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client ()
import Network.HTTP.Types.Status (status200, status400, status500)
import Network.Wai
       ( Application
       , consumeRequestBodyStrict
       , requestMethod
       , responseLBS
       )
import Network.Wai.Handler.Warp
       (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import System.Timeout (timeout)

import Smarrow.Deploy.Event
import Smarrow.Deploy.EventQueue
import Smarrow.Deploy.PendingRequests

------------------------------------------------------------------------

data Envelope = Envelope
  { receiver :: ByteString
  , tag      :: ByteString
  , payload  :: ByteString
  }

data Transport = Transport
  { tRespond :: ClientId -> ByteString -> IO ()
  , tStart   :: EventQueue -> MVar () -> IO ()
  }

newHttpTransport :: Int -> IO Transport
newHttpTransport port = do
  pr <- emptyPendingRequests
  return Transport
    { tRespond = respondToPendingRequest pr
    , tStart   = \eq ready -> runSettings (settings ready) (app pr eq)
    }
    where
      settings ready
         = setPort port
         $ setBeforeMainLoop (putMVar ready ())
         $ defaultSettings

newtype NodeId = NodeId Int -- XXX

app :: PendingRequests -> EventQueue -> Application
app pr eq req respond =
  case requestMethod req of
     "POST"     -> request InputEv
     "PUT"      -> request SpawnEv
     "PATCH"    -> request UpgradeEv
     _otherwise -> respond (responseLBS status400 [] "Unsupported method")
  where
    request ev = do
      reqBody <- consumeRequestBodyStrict req
      (fromClientId, resp) <- addPendingRequest pr
      -- XXX: time <- cGetCurrentTime clock
      eqEnqueue eq (ev fromClientId (LBS.toStrict reqBody))
      mBs <- timeout (60_000_000) (takeMVar resp) -- 60s
      removePendingRequest pr fromClientId
      case mBs of
        Nothing -> do
          putStrLn "Client response timed out..."
          respond (responseLBS status500 [] "Timeout due to overload or bug")
        Just bs -> respond (responseLBS status200 [] (LBS.fromStrict bs))
