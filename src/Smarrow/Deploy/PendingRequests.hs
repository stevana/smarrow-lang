module Smarrow.Deploy.PendingRequests where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef

------------------------------------------------------------------------

newtype ClientId = ClientId Int

data PendingRequests = PendingRequests
  { prPendingRequests :: IORef (IntMap (MVar ByteString))
  , prNextClientId    :: IORef Int
  }

emptyPendingRequests :: IO PendingRequests
emptyPendingRequests = PendingRequests <$> newIORef IntMap.empty <*> newIORef 0

addPendingRequest :: PendingRequests -> IO (ClientId, MVar ByteString)
addPendingRequest pr = do
  i <- atomicModifyIORef' (prNextClientId pr) (\i -> (i + 1, i))
  resp <- newEmptyMVar
  atomicModifyIORef' (prPendingRequests pr) (\im -> (IntMap.insert i resp im, ()))
  return (ClientId i, resp)

removePendingRequest :: PendingRequests -> ClientId -> IO ()
removePendingRequest pr (ClientId cid) =
  atomicModifyIORef' (prPendingRequests pr) (\im -> (IntMap.delete cid im, ()))

respondToPendingRequest :: PendingRequests -> ClientId -> ByteString -> IO ()
respondToPendingRequest pr (ClientId cid) resp = do
  im <- readIORef (prPendingRequests pr)
  case IntMap.lookup cid im of
    Nothing   -> putStrLn ("respondToPendingRequest, client not found: " ++ show cid)
    Just mvar -> putMVar mvar resp
