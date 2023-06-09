{-# LANGUAGE StrictData #-}

module Smarrow.Deploy.Event where

import Data.ByteString (ByteString)

import Smarrow.CCC
import Smarrow.Deploy.PendingRequests (ClientId)
import Smarrow.Deploy.Config (SMId)
import Smarrow.Value

------------------------------------------------------------------------

data Event
  = InputEv   ClientId ByteString
  | SpawnEv   ByteString
  | UpgradeEv ByteString
  | QuitEv
