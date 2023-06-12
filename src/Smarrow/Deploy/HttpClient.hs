{-# LANGUAGE StrictData #-}

module Smarrow.Deploy.HttpClient where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
       ( Manager
       , Request
       , RequestBody(..)
       , defaultManagerSettings
       , httpLbs
       , method
       , newManager
       , parseRequest
       , requestBody
       , responseBody
       , responseStatus
       )
import Network.HTTP.Types.Status

import Smarrow.CCC
import Smarrow.Deploy.Codec
import Smarrow.Deploy.Config
import Smarrow.Value

------------------------------------------------------------------------

data Client = Client
  { cManager :: Manager
  , cRequest :: Request
  , cCodec   :: Codec
  }

newClient :: String -> Codec -> IO Client
newClient host c = do
  mgr <- newManager defaultManagerSettings
  req <- parseRequest host
  return (Client mgr req c)

call :: Client -> SMId -> Value -> IO ByteString
call (Client mgr req c) smid input = do
  let body = RequestBodyBS (cEncodeInput c (Input smid input))
  resp <- httpLbs req { method = "POST", requestBody = body } mgr
  return (responseBody resp)

spawn :: Client -> SMId -> CCC -> Value -> IO ()
spawn (Client mgr req c) smid code state = do
  let body = RequestBodyBS (cEncodeSpawn c (Spawn smid code state))
  resp <- httpLbs req { method = "PUT", requestBody = body } mgr
  if responseStatus resp == ok200
  then return ()
  else error "spawn: failed" -- XXX: error msg?
