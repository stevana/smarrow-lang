module Smarrow.Deploy.EventQueue where

import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM (atomically)

import Smarrow.Deploy.Event

------------------------------------------------------------------------

data EventQueue = EventQueue
  { eqEnqueue :: Event -> IO ()
  , eqDequeue :: IO Event
  }

newStmEventQueue :: IO EventQueue
newStmEventQueue = do
  q <- newTBQueueIO 128
  return EventQueue
    { eqEnqueue = atomically . writeTBQueue q
    , eqDequeue = atomically (readTBQueue q)
    }
