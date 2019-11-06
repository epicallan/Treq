{-# LANGUAGE TupleSections #-}
module Network.HTTP.Hreq.Internal
  ( Hreq (..)
  , RunHttp (..)
  , runHreq
  , runHreqWithConfig
  ) where

import Prelude ()
import Prelude.Compat

import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVar, writeTVar)
import Control.Monad.Catch (SomeException (..), catch, throwM)
import Control.Monad.IO.Unlift (MonadUnliftIO (..), wrappedWithRunInIO)
import Control.Monad.Reader (MonadIO (..), MonadReader, MonadTrans, ReaderT (..), ask, asks)
import Control.Monad.STM (STM, atomically)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (maybeToList)
import Data.Foldable (toList)
import Data.String.Conversions (cs)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types (Header, hAccept, hContentType, renderQuery, statusCode)

import Network.Core.Http
import Network.HTTP.Hreq.Config

-- | Monad for running Http client requests
newtype Hreq m a = Hreq { runHreq' :: ReaderT HttpConfig m a }
  deriving (Functor, Applicative, Monad, MonadReader HttpConfig, MonadTrans, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (Hreq m) where
  withRunInIO = wrappedWithRunInIO Hreq runHreq'

instance RunHttp (Hreq IO) where
  runHttp req = do
    config <- ask

    let manager = httpManager config
    let mcookieJar = httpCookieJar config
    let httpRequest = requestToHTTPRequest (httpBaseUrl config) req

    ehttpResponse <- liftIO . catchConnectionError
                       $ performHttpRequest httpRequest manager mcookieJar

    response <- either throwHttpError (pure . httpResponsetoResponse) ehttpResponse

    maybe (pure response) throwHttpError =<< checkResponse req response

  throwHttpError err = Hreq $ throwM err >>= return

  checkResponse req response = do
    statusRange <- asks httpStatuses
    let code = statusCode $ resStatus response
    pure $ if code >= statusLower statusRange && code <= statusUpper statusRange
      then Just $ FailureResponse req response
      else Nothing

runHreq :: MonadIO m => BaseUrl -> Hreq m a -> m a
runHreq baseUrl action = do
  config <- liftIO $ createDefConfig baseUrl

  runHreqWithConfig config action

runHreqWithConfig :: HttpConfig -> Hreq m a -> m a
runHreqWithConfig config action = runReaderT (runHreq' action) config

performHttpRequest
  :: HTTP.Request
  -> HTTP.Manager
  -> Maybe (TVar HTTP.CookieJar)
  -> IO (HTTP.Response LBS.ByteString)
performHttpRequest request manager mcookieJar = case mcookieJar of
  Nothing -> HTTP.httpLbs request manager
  Just cj -> do
    req' <- cookieJarRequest cj request
    HTTP.withResponseHistory req' manager $ updateWithResponseCookies cj
  where
    cookieJarRequest :: TVar HTTP.CookieJar -> HTTP.Request -> IO HTTP.Request
    cookieJarRequest cj req = do
      now <- getCurrentTime
      atomically $ do
        oldCookieJar <- readTVar cj
        let (newReq, newCookieJar) = HTTP.insertCookiesIntoRequest req oldCookieJar now
        writeTVar cj newCookieJar
        pure newReq

    -- updateWithResponseCookies code is borrowed from servant-client
    updateWithResponseCookies
      :: TVar HTTP.CookieJar
      -> HTTP.HistoriedResponse HTTP.BodyReader
      -> IO (HTTP.Response LBS.ByteString)
    updateWithResponseCookies cj responses = do
        now <- getCurrentTime
        bss <- HTTP.brConsume $ HTTP.responseBody fRes
        let fRes'        = fRes { HTTP.responseBody = LBS.fromChunks bss }
            allResponses = HTTP.hrRedirects responses <> [(fReq, fRes')]
        atomically $ mapM_ (updateCookieJar now) allResponses
        return fRes'
      where
          updateCookieJar :: UTCTime -> (HTTP.Request, HTTP.Response LBS.ByteString) -> STM ()
          updateCookieJar now' (req', res') = modifyTVar' cj (fst . HTTP.updateCookieJar res' req' now')

          fReq = HTTP.hrFinalRequest responses
          fRes = HTTP.hrFinalResponse responses

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
    , HTTP.queryString = renderQuery True $ toList $ reqQueryString r
    , HTTP.requestHeaders = maybeToList acceptHeader <> maybeToList contentType <> headers
    , HTTP.requestBody = body
    , HTTP.secure = isSecure
    }
  where
    headers :: [ Header ]
    headers = filter ( \(hname, _) -> hname /= hAccept && hname /= hContentType)
            $ toList $ reqHeaders r

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
