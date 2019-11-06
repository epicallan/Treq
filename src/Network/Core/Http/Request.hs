{-# LANGUAGE DeriveFunctor #-}
module Network.Core.Http.Request where

import Prelude ()
import Prelude.Compat

import Data.ByteString as B
import qualified Data.Sequence as Seq
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
  , reqQueryString :: Seq.Seq QueryItem
  , reqHttpVersion :: HttpVersion
  , reqAccept      :: Maybe MediaType
  , reqHeaders     :: Seq.Seq Header
  } deriving (Eq, Show, Generic, Functor, Typeable)

type Request= RequestF ByteString

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

setReqBody :: ByteString -> MediaType -> Request -> Request
setReqBody body mediaType req =
  req { reqBody = Just (body, mediaType) }
