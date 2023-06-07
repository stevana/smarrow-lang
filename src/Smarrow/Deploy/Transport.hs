{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Smarrow.Deploy.Transport where

import Control.Concurrent.MVar
import Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Read (decimal)
import Network.HTTP.Client ()
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
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
  , tStart   :: Int -> EventQueue -> IO ()
  }

newHttpTransport :: IO Transport
newHttpTransport = do
  pr <- emptyPendingRequests
  return Transport
    { tRespond = respondToPendingRequest pr
    , tStart   = \port eq -> run port (app pr eq)
    }

newtype NodeId = NodeId Int -- XXX

app :: PendingRequests -> EventQueue -> Application
app pr eq req respond =
  case requestMethod req of
     "POST" -> case parseNodeId of
                 Nothing -> respond (responseLBS status400 [] "Missing receiver node id")
                 Just toNodeId -> do
                   reqBody <- consumeRequestBodyStrict req
                   (fromClientId, resp) <- addPendingRequest pr
                   -- time <- cGetCurrentTime clock
                   eqEnqueue eq (InputEv fromClientId (LBS.toStrict reqBody))
                     -- (NetworkEvent toNodeId (ClientRequest time fromClientId reqBody)))
                   mBs <- timeout (60_000_000) (takeMVar resp) -- 60s
                   removePendingRequest pr fromClientId
                   case mBs of
                     Nothing -> do
                       putStrLn "Client response timed out..."
                       respond (responseLBS status500 [] "Timeout due to overload or bug")
                     Just bs -> respond (responseLBS status200 [] (LBS.fromStrict bs))
     _otherwise -> respond (responseLBS status400 [] "Unsupported method")
   where
     parseNodeId :: Maybe NodeId
     parseNodeId =
       case pathInfo req of
         [txt] -> case decimal txt of
           Right (nodeId, _rest) -> Just (NodeId nodeId)
           _otherwise -> Nothing
         _otherwise   -> Nothing
