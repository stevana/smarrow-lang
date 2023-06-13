{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smarrow.Deploy.Config where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)

import Smarrow.CCC
import Smarrow.Interpreter
import Smarrow.Value

------------------------------------------------------------------------

newtype SMId = SMId ByteString
  deriving (Eq, Ord, Show, Read, IsString)

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
stepSM smid input cfg =
  let
    sm                  = cConfig cfg Map.! smid
    PairV state' output = run (smCode sm) input (smState sm)
  in
    (cfg, output)

spawnSM :: SMId -> CCC -> Value -> Config -> Config
spawnSM smid code state cfg =
  cfg { cConfig = Map.insert smid (StateMachine code state) (cConfig cfg) }

upgradeSM :: SMId -> CCC -> CCC -> CCC -> Config -> Config
upgradeSM smid oldCode newCode stateMigration cfg =
  cfg { cConfig = Map.insert smid (StateMachine newCode migratedState) (cConfig cfg) }
  where
    sm = cConfig cfg Map.! smid
    -- XXX: Support for migrations that use state?
    PairV _unit migratedState = run stateMigration (smState sm) UnitV
