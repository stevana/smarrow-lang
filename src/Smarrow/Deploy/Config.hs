{-# LANGUAGE StrictData #-}

module Smarrow.Deploy.Config where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Smarrow.CCC
import Smarrow.Interpreter
import Smarrow.Value

------------------------------------------------------------------------

data Config = Config
  { cConfig :: Map SMId StateMachine
  }

type SMId = ByteString

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
