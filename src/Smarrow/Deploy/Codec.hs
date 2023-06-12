module Smarrow.Deploy.Codec where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Text.Read

import Smarrow.Value
import Smarrow.CCC
import Smarrow.Deploy.Config (SMId)

------------------------------------------------------------------------

data Codec = Codec
  { cDecodeInput  :: ByteString -> Either String Input
  , cEncodeInput  :: Input -> ByteString
  , cEncodeOutput :: Value -> ByteString
  , cDecodeSpawn  :: ByteString -> Either String Spawn
  , cEncodeSpawn  :: Spawn -> ByteString
  }

data Input = Input { iReceiver :: SMId, iInput :: Value }
  deriving (Show, Read)

data Spawn = Spawn { sSMId :: SMId, sCode :: CCC, sInitialState :: Value }
  deriving (Show, Read)

readShowCodec :: Codec
readShowCodec = Codec
  { cDecodeInput  = readEither . BS8.unpack
  , cEncodeInput  = BS8.pack . show
  , cEncodeOutput = BS8.pack . show
  , cDecodeSpawn  = readEither . BS8.unpack
  , cEncodeSpawn  = BS8.pack . show
  }
