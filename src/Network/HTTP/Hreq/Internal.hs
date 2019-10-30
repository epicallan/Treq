{-# LANGUAGE TupleSections #-}
module Network.HTTP.Hreq.Internal where

import Prelude ()
import Prelude.Compat

import Control.Monad.Catch (SomeException (..), catch, throwM)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (maybeToList)
import Data.String.Conversions (cs)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types (Header, hAccept, hContentType, renderQuery, statusCode)

import Network.Core.Http
import Network.HTTP.Hreq.Config

newtype Hreq m a = Hreq { runHreq' :: ReaderT HttpConfig m a }
  deriving (Functor, Applicative, Monad, MonadReader HttpConfig, MonadTrans, MonadIO)

instance RunHttp (Hreq IO) where
  runHttp req = do
    config <- ask

    let manager = httpManager config
    let httpRequest = requestToHTTPRequest (httpBaseUrl config) req

    ehttpResponse <- liftIO . catchConnectionError $ HTTP.httpLbs httpRequest manager

    response <- either throwHttpError (pure . httpResponsetoResponse) ehttpResponse

    maybe (pure response) throwHttpError =<< checkResponse req response

  throwHttpError err = Hreq $ throwM err >>= return

  checkResponse req response = do
    statusRange <- asks httpStatuses
    let code = statusCode $ resStatus response
    pure $ if code >= statusLower statusRange && code <= statusUpper statusRange
      then Just $ FailureResponse req response
      else Nothing


-- TODO: MonadUnliftIO or MonadBaseControl instances

runHreq :: MonadIO m => BaseUrl -> Hreq m a -> m a
runHreq baseUrl action = do
  config <- liftIO $ createDefConfig baseUrl

  runHreqWithConfig config action

runHreqWithConfig :: HttpConfig -> Hreq m a -> m a
runHreqWithConfig config action = runReaderT (runHreq' action) config

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

catchConnectionError :: IO a -> IO (Either HttpError a)
catchConnectionError action =
  catch (Right <$> action)
    $ \e -> pure . Left . ConnectionError $ SomeException (e :: HTTP.HttpException)
