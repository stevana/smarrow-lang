module Smarrow.Deploy.Codec where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Text.Read

import Smarrow.Value
import Smarrow.CCC
import Smarrow.Deploy.Config (SMId)

------------------------------------------------------------------------

data Codec = Codec
  { cDecodeInput   :: ByteString -> Either String Input
  , cEncodeInput   :: Input -> ByteString
  , cDecodeOutput  :: ByteString -> Either String Output
  , cEncodeOutput  :: Output -> ByteString
  , cDecodeSpawn   :: ByteString -> Either String Spawn
  , cEncodeSpawn   :: Spawn -> ByteString
  , cDecodeUpgrade :: ByteString -> Either String Upgrade
  , cEncodeUpgrade :: Upgrade -> ByteString
  }

data Input = Input
  { iReceiver :: SMId
  , iInput    :: Value
  }
  deriving (Show, Read)

data Output = Output
  { oOutput   :: Value
  }
  deriving (Show, Read)

data Spawn = Spawn
  { sSMId         :: SMId
  , sCode         :: CCC
  , sInitialState :: Value
  }
  deriving (Show, Read)

data Upgrade = Upgrade
  { uSMId           :: SMId
  , uOldCode        :: CCC -- XXX: hash of old code should be enough?
  , uNewCode        :: CCC
  , uStateMigration :: CCC
  }
  deriving (Show, Read)

readShowCodec :: Codec
readShowCodec = Codec
  { cDecodeInput   = decode
  , cEncodeInput   = encode
  , cDecodeOutput  = decode
  , cEncodeOutput  = encode
  , cDecodeSpawn   = decode
  , cEncodeSpawn   = encode
  , cDecodeUpgrade = decode
  , cEncodeUpgrade = encode
  }
  where
    encode :: Show a => a -> ByteString
    encode = BS8.pack . show

    decode :: Read a => ByteString -> Either String a
    decode = readEither . BS8.unpack
