-- | This module contains a collection of some of the Internet Media or Mime types
-- and class to serialize and deserialize them.
-- At the moment we only support a small set but its possible to write own custom
-- types and provide the required instances.
module Network.Core.API.MediaType
  ( module Network.Core.API.MediaType
  , MediaType
  , (//)
  , matches
  , parseAccept
  )where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Media (MediaType, matches, parseAccept, (//), (/:))
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)

-- * Provided Content types
data JSON deriving Typeable
data PlainText deriving Typeable
data FormUrlEncoded deriving Typeable

newtype DecodeError = DecodeError { unDecodeError :: Text }
  deriving (Show, Eq)

instance Exception DecodeError

-- | Instances of 'HasMediaType' are useful for matching against the @Accept@ HTTP header
-- of the request and setting @Content-Type@ header of the response
class HasMediaType ctyp where
  mediaType :: sing ctyp -> MediaType
  mediaType = NE.head . mediaTypes

  mediaTypes :: sing ctyp -> NE.NonEmpty MediaType
  mediaTypes = (NE.:| []) . mediaType

  {-# MINIMAL mediaType | mediaTypes #-}

instance HasMediaType JSON where
  mediaTypes _ =
    "application" // "json" /: ("charset", "utf-8") NE.:|
    [ "application" // "json" ]

instance HasMediaType PlainText where
  mediaType _ = "text" // "plain" /: ("charset", "utf-8")

instance HasMediaType FormUrlEncoded where
  mediaType _ = "application" // "x-www-form-urlencoded"

class HasMediaType ctyp => MediaDecode ctyp a where
  mediaDecode :: sing ctyp -> ByteString -> Either DecodeError a

class HasMediaType ctyp => MediaEncode ctyp a where
  mediaEncode :: sing ctyp -> a -> ByteString

instance FromJSON a => MediaDecode JSON a where
  mediaDecode _ = first (DecodeError . cs) . eitherDecodeStrict

instance ToJSON a => MediaEncode JSON a where
  mediaEncode _ = cs . encode

instance MediaDecode PlainText Text where
  mediaDecode _ = Right . cs

instance Show a => MediaEncode PlainText a where
  mediaEncode _ = cs . show

instance ToForm a => MediaEncode FormUrlEncoded a where
  mediaEncode _ = cs . urlEncodeAsForm

instance FromForm a => MediaDecode FormUrlEncoded a where
  mediaDecode _ = first DecodeError . urlDecodeAsForm . cs
