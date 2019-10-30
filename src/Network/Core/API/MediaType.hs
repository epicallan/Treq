module Network.Core.API.MediaType
  ( module Network.Core.API.MediaType
  , MediaType
  )where

import Control.Exception
import Data.Aeson as A
import Data.Bifunctor
import Data.ByteString as B
import Data.String.Conversions
import Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Media (MediaType, (//))

data JSON
data PlainText

newtype DecodeError = DecodeError { unDecodeError :: Text }
  deriving (Show, Eq)

instance Exception DecodeError

class HasMediaType ctyp where
  mediaType :: sing ctyp -> MediaType

instance HasMediaType JSON where
  mediaType _ = "application" // "json"

instance HasMediaType PlainText where
  mediaType _ = "text" // "plain"

class HasMediaType ctyp => MediaDecode ctyp a where
  decode :: sing ctyp -> B.ByteString -> Either DecodeError a

instance A.FromJSON a => MediaDecode JSON a where
  decode _ = first (DecodeError . T.pack) . A.eitherDecodeStrict

instance MediaDecode PlainText Text where
  decode _ = Right . decodeUtf8

class HasMediaType ctyp => MediaEncode ctyp a where
  encode :: sing ctyp -> a -> B.ByteString

instance Show a => MediaEncode PlainText a where
  encode _ = encodeUtf8 . T.pack . show

instance A.ToJSON a => MediaEncode JSON a where
  encode _ = cs . A.encode
