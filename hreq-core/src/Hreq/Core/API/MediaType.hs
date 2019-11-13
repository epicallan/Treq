-- | This module contains a collection of some of the Internet Media or Mime types
-- and class to serialize and deserialize them.
-- At the moment we only support a small set but its possible to write own custom
-- types and provide the required instances.
module Hreq.Core.API.MediaType
  ( module Hreq.Core.API.MediaType
  , MediaType
  , (//)
  , matches
  , parseAccept
  )where

import Prelude ()
import Prelude.Compat

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON, encode, parseJSON)
import Data.Aeson.Parser (value)
import Data.Aeson.Types (parseEither)
import Data.Attoparsec.ByteString.Char8 (endOfInput, parseOnly, skipSpace, (<?>))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Media (MediaType, matches, parseAccept, (//), (/:))
import Text.Read (readMaybe)
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)

-- * Provided Content types
data JSON deriving Typeable
data PlainText deriving Typeable
data OctetStream deriving Typeable
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

instance HasMediaType OctetStream where
  mediaType _ = "application" // "octet-stream"

class HasMediaType ctyp => MediaDecode ctyp a where
  mediaDecode :: sing ctyp -> LBS.ByteString -> Either DecodeError a

class HasMediaType ctyp => MediaEncode ctyp a where
  mediaEncode :: sing ctyp -> a -> LBS.ByteString

instance FromJSON a => MediaDecode JSON a where
  mediaDecode _ = first (DecodeError . cs) . eitherDecodeLenient

instance ToJSON a => MediaEncode JSON a where
  mediaEncode _ = cs . encode

instance MediaDecode OctetStream ByteString where
  mediaDecode _ = Right . cs

instance MediaDecode OctetStream LBS.ByteString where
  mediaDecode _ = Right . id

instance MediaEncode OctetStream ByteString where
  mediaEncode _ = cs

instance MediaEncode OctetStream LBS.ByteString where
  mediaEncode _ = id

instance MediaDecode PlainText Text where
  mediaDecode _ = Right . cs

instance Read a => MediaDecode PlainText a where
  mediaDecode _ bs =
    maybe (Left $ DecodeError $ "Failed to decode: " <> cs bs) Right . readMaybe $ cs bs

instance Show a => MediaEncode PlainText a where
  mediaEncode _ = cs . show

instance ToForm a => MediaEncode FormUrlEncoded a where
  mediaEncode _ = cs . urlEncodeAsForm

instance FromForm a => MediaDecode FormUrlEncoded a where
  mediaDecode _ = first DecodeError . urlDecodeAsForm . cs

-- * Helper functions

-- | Like 'Data.Aeson.eitherDecode' but allows all JSON values instead of just
-- objects and arrays. This function is borrowed from @servant@
--
-- Will handle trailing whitespace, but not trailing junk. ie.
--
-- >>> eitherDecodeLenient "1 " :: Either String Int
-- Right 1
--
-- >>> eitherDecodeLenient "1 junk" :: Either String Int
-- Left "trailing junk after valid JSON: endOfInput"
eitherDecodeLenient :: FromJSON a => LBS.ByteString -> Either String a
eitherDecodeLenient input =
    parseOnly parser (cs input) >>= parseEither parseJSON
  where
    parser = skipSpace
          *> Data.Aeson.Parser.value
          <* skipSpace
          <* (endOfInput <?> "trailing junk after valid JSON")
