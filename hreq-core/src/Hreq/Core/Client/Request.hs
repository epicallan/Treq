-- | This module provides a 'Request' data type which contains components required for
-- creation of an HTTP Request.
--
-- 'Request' data is built from type level API endpoints and the 'Hreq.Core.Client.BaseUrl.BaseUrl'
-- with in the 'Hreq.Core.API.HasRequest.HasRequest' class instance.
--
{-# LANGUAGE DeriveFunctor #-}
module Hreq.Core.Client.Request where

import Prelude ()
import Prelude.Compat

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as Seq
import Data.Text
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types (Header, HeaderName, HttpVersion (..), Method, QueryItem, http11,
                           methodGet)
import Web.HttpApiData (ToHttpApiData (..))

import Hreq.Core.API.Streaming (GivesPooper)

-- * Request
data RequestF body = Request
  { reqPath        :: Text
  , reqMethod      :: Method
  , reqBody        :: Maybe (body, MediaType)
  , reqQueryString :: Seq.Seq QueryItem
  , reqHttpVersion :: HttpVersion
  , reqAccept      :: Maybe MediaType
  , reqHeaders     :: Seq.Seq Header
  } deriving (Show, Eq, Functor)


-- | The Request body replica of the @http-client@ @RequestBody@.
data RequestBody =
    RequestBodyLBS LBS.ByteString
  | RequestBodyBS ByteString
  | RequestBodyStream (GivesPooper ())
  deriving (Show, Eq)


type Request = RequestF RequestBody

-- * Default Request
defaultRequest :: Request
defaultRequest = Request
  { reqPath = ""
  , reqMethod = methodGet
  , reqBody = Nothing
  , reqQueryString = Seq.empty
  , reqHttpVersion = http11
  , reqAccept = Nothing
  , reqHeaders = Seq.empty
  }

-- * Request helper functions
appendMethod :: Method -> Request -> Request
appendMethod method req = req { reqMethod = method }

appendToPath :: Text -> Request -> Request
appendToPath p req = req { reqPath = reqPath req <> "/" <> p }

appendToQueryString
  :: QueryItem
  -> Request
  -> Request
appendToQueryString queryItem req =
  req { reqQueryString =  reqQueryString req Seq.|> queryItem }

addHeader :: ToHttpApiData a => HeaderName -> a -> Request -> Request
addHeader name val req =
  req {reqHeaders = reqHeaders req Seq.|> (name, toHeader val) }

setReqBody :: RequestBody -> MediaType -> Request -> Request
setReqBody body mediaType req =
  req { reqBody = Just (body, mediaType) }


-- setReqBody :: ByteString -> MediaType -> Request -> Request
-- setReqBody body mediaType req =
--   req { reqBody = Just (body, mediaType) }
