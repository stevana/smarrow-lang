{-# LANGUAGE StrictData #-}

module Smarrow.Deploy.Event where

import Data.ByteString (ByteString)

import Smarrow.Deploy.PendingRequests (ClientId)

------------------------------------------------------------------------

data Event
  = InputEv ClientId ByteString
  | QuitEv
