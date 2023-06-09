{-# LANGUAGE StrictData #-}

module Smarrow.Deploy.HttpClient where

import Network.HTTP.Client

import Smarrow.Deploy.Codec

------------------------------------------------------------------------

data Client = Client
  { cManager :: Manager
  , cRequest :: Request
  }

newClient :: String -> IO Client
newClient host = do
  mgr <- newManager defaultManagerSettings
  req <- parseRequest host
  return (Client mgr req)

call :: Client -> Input -> IO ()
call c (Input smId input)= undefined
