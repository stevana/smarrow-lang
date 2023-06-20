{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Smarrow.Deploy.HttpClient where

import qualified Data.ByteString.Lazy as LBS
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

import Smarrow.Deploy.Codec
import Smarrow.Deploy.Config
import Smarrow.AST

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

newTestClient :: Int -> IO Client
newTestClient port = do
  mgr <- newManager defaultManagerSettings
  req <- parseRequest ("http://localhost:" ++ show port)
  return (Client mgr req readShowCodec)

call :: Client -> SMId -> Value -> IO (Either String Output)
call (Client mgr req c) smid input = do
  let body = RequestBodyBS (cEncodeInput c (Input smid input))
  resp <- httpLbs req { method = "POST", requestBody = body } mgr
  return (cDecodeOutput c (LBS.toStrict (responseBody resp)))

call_ :: Client -> SMId -> Value -> IO Value
call_ cid smid input = do
  r <- call cid smid input
  case r of
    Left  err             -> error err
    Right (Output output) -> return output

spawn :: Client -> SMId -> CCC -> Value -> LangDecl -> IO ()
spawn (Client mgr req c) smid code state lang = do
  let body = RequestBodyBS (cEncodeSpawn c (Spawn smid code state lang))
  resp <- httpLbs req { method = "PUT", requestBody = body } mgr
  if responseStatus resp == ok200
  then return ()
  else error "spawn: failed" -- XXX: error msg?

upgrade :: Client -> SMId -> CCC -> CCC -> CCC -> LangDecl -> IO ()
upgrade (Client mgr req c) smid oldCode newCode stateMigration newLang = do
  let body = RequestBodyBS (cEncodeUpgrade c (Upgrade smid oldCode newCode stateMigration newLang))
  resp <- httpLbs req { method = "PATCH", requestBody = body } mgr
  if responseStatus resp == ok200
  then return ()
  else error "upgrade: failed" -- XXX: error msg?
