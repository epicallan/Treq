module Network.Core.Http.Request where

import Prelude ()
import Prelude.Compat

import Data.ByteString as B
import GHC.Generics
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types (Header, HeaderName, HttpVersion (..), Method, QueryItem, http11,
                           methodGet)
import Web.HttpApiData (ToHttpApiData (..))

data Request = Request
  { reqPath        :: String -- | TODO: Maybe use builder or Text
  , reqMethod      :: Method
  , reqBody        :: Maybe (B.ByteString, MediaType)
  , reqQueryString :: [QueryItem]
  , reqHttpVersion :: HttpVersion
  , reqAccept      :: Maybe MediaType
  , reqHeaders     :: [Header]
  } deriving (Eq, Show, Generic)

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

appendToPath :: String -> Request -> Request
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
