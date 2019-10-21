{-# LANGUAGE TupleSections #-}
module Network.HTTP.Hreq.Internal where

import Control.Exception.Safe (MonadThrow, throwM)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (maybeToList)
import Data.Singletons.TypeRepTYPE ()
import Data.String.Conversions (cs)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types (Header, hAccept, hContentType, renderQuery)

import Network.Core.Http
import Network.HTTP.Hreq.Config

newtype Hreq m a = Hreq { runHreq' :: ReaderT HttpConfig m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HttpConfig, MonadThrow)

instance RunHttp (Hreq IO) where
  runRequest req = do
    config <- ask
    let manager' = httpManager config
    let req' = requestToHTTPRequest (httpBaseUrl config) req

    httpResponse <- liftIO $ HTTP.httpLbs req' manager'
    return $ httpResponsetoResponse httpResponse

  throwHttpError = throwM

-- TODO: MonadBaseControl instances

runHreq :: MonadIO m => BaseUrl -> Hreq m a -> m a
runHreq baseUrl hrequest = do
  config <- liftIO $ createDefConfig baseUrl

  runHreqWithConfig config hrequest

runHreqWithConfig :: HttpConfig -> Hreq m a -> m a
runHreqWithConfig config hrequest = runReaderT (runHreq' hrequest) config

httpResponsetoResponse :: HTTP.Response LBS.ByteString -> Response
httpResponsetoResponse response = Response
 { resStatus = HTTP.responseStatus response
 , resHeaders = HTTP.responseHeaders response
 , resBody = cs $ HTTP.responseBody response
 , resHttpVersion = HTTP.responseVersion response
 }

requestToHTTPRequest :: BaseUrl -> Request -> HTTP.Request
requestToHTTPRequest burl r = HTTP.defaultRequest
    { HTTP.method = reqMethod r
    , HTTP.host = cs $ baseUrlHost burl
    , HTTP.port = baseUrlPort burl
    , HTTP.path = cs $ baseUrlPath burl <> reqPath r
    , HTTP.queryString = renderQuery True $ reqQueryString r
    , HTTP.requestHeaders = maybeToList acceptHeader <> maybeToList contentType <> headers
    , HTTP.requestBody = body
    , HTTP.secure = isSecure
    }
  where
    headers :: [ Header ]
    headers = filter ( \(hname, _) -> hname /= hAccept && hname /= hContentType)
            $ reqHeaders r

    acceptHeader :: Maybe Header
    acceptHeader = (hAccept, ) . renderHeader <$> reqAccept r

    (body, contentType) = case reqBody r of
      Nothing -> (HTTP.RequestBodyBS mempty, Nothing)
      Just (body', ctyp) ->
        (HTTP.RequestBodyBS body', Just (hContentType, renderHeader ctyp))

    isSecure :: Bool
    isSecure = case baseUrlScheme burl of
        Http  -> False
        Https -> True
