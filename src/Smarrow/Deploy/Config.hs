{-# LANGUAGE StrictData #-}

module Smarrow.Deploy.Config where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Smarrow.CCC
import Smarrow.Interpreter
import Smarrow.Value

------------------------------------------------------------------------

newtype SMId = SMId ByteString
  deriving (Eq, Ord, Read)

data Config = Config
  { cConfig :: Map SMId StateMachine
  }

data StateMachine = StateMachine
  { smCode  :: CCC
  , smState :: Value
  }

------------------------------------------------------------------------

emptyConfig :: Config
emptyConfig = Config
  { cConfig = Map.empty
  }

stepSM :: SMId -> Value -> Config -> (Config, Value)
stepSM smId input cfg =
  let
    sm                  = cConfig cfg Map.! smId
    PairV state' output = run (smCode sm) input (smState sm)
  in
    (cfg, output)

spawnSM :: SMId -> CCC -> Value -> Config -> Config
spawnSM smId code state cfg =
  cfg { cConfig = Map.insert smId (StateMachine code state) (cConfig cfg) }
