{-# LANGUAGE DeriveFunctor #-}
module Network.Core.Http.Request where

import Prelude ()
import Prelude.Compat

import Data.ByteString as B
import Data.Text
import Data.Typeable
import GHC.Generics
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types (Header, HeaderName, HttpVersion (..), Method, QueryItem, http11,
                           methodGet)
import Web.HttpApiData (ToHttpApiData (..))

data RequestF body = Request
  { reqPath        :: Text
  , reqMethod      :: Method
  , reqBody        :: Maybe (body, MediaType)
  , reqQueryString :: [QueryItem]
  , reqHttpVersion :: HttpVersion
  , reqAccept      :: Maybe MediaType
  , reqHeaders     :: [Header]
  } deriving (Eq, Show, Generic, Functor, Typeable)

type Request= RequestF ByteString

defaultRequest :: Request
defaultRequest = Request
  { reqPath = ""
  , reqMethod = methodGet
  , reqBody = Nothing
  , reqQueryString = []
  , reqHttpVersion = http11
  , reqAccept = Nothing
  , reqHeaders = []
  }

appendMethod :: Method -> Request -> Request
appendMethod method req = req { reqMethod = method }

appendToPath :: Text -> Request -> Request
appendToPath p req = req { reqPath = reqPath req <> p }

appendToQueryString
  :: QueryItem
  -> Request
  -> Request
appendToQueryString queryItem req =
  req { reqQueryString = queryItem : reqQueryString req }

addHeader :: ToHttpApiData a => HeaderName -> a -> Request -> Request
addHeader name val req =
  req {reqHeaders = (name, toHeader val) : reqHeaders req}

setReqBody :: ByteString -> MediaType -> Request -> Request
setReqBody body mediaType req =
  req { reqBody = Just (body, mediaType) }
