module Smarrow.Deploy.Codec where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

import Smarrow.Value
import Smarrow.Parser

------------------------------------------------------------------------

data Codec = Codec
  { cDecode :: ByteString -> Either String Value
  , cEncode :: Value -> ByteString
  }

parserPrettyCodec :: Codec
parserPrettyCodec = Codec
  { cDecode = runParser_ pValue'
  , cEncode = BS8.pack . show -- XXX: implement pretty printer
  }
