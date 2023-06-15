{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smarrow.Deploy.Config where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)

import Smarrow.CCC
import Smarrow.Interpreter
import Smarrow.Value

------------------------------------------------------------------------

newtype SMId = SMId ByteString
  deriving (Eq, Ord, Show, Read, IsString)

displaySMId :: SMId -> String
displaySMId (SMId bs) = BS8.unpack bs

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
    sm                   = cConfig cfg Map.! smid
    PairV state' output  = run (smCode sm) input (smState sm)
    cfg' = cfg { cConfig = Map.insert smid sm { smState = state' } (cConfig cfg) }
  in
    (cfg', output)

spawnSM :: SMId -> CCC -> Value -> Config -> Config
spawnSM smid code state cfg =
  cfg { cConfig = Map.insert smid (StateMachine code state) (cConfig cfg) }

-- XXX: Check if we are running oldCode.
upgradeSM :: SMId -> CCC -> CCC -> CCC -> Config -> Config
upgradeSM smid oldCode newCode stateMigration cfg =
  cfg { cConfig = Map.insert smid (StateMachine newCode migratedState) (cConfig cfg) }
  where
    sm = cConfig cfg Map.! smid
    -- XXX: Support for migrations that use state?
    -- XXX: Partial, return tuple instead...
    PairV _unit migratedState = run stateMigration (smState sm) UnitV
