{-# LANGUAGE AllowAmbiguousTypes #-}
module Hreq.Conduit.Internal.HTTP
  ( Hreq (..)
  , ResBodyStream (..)
  , RunConduitClient
  -- * Run Hreq client
  , runHreq
  , runHreqWithConfig
  , createDefConfig
  -- * creates Hreq Client
  , hreq
  , hreqWithConduit
  ) where
import Control.Monad
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..), wrappedWithRunInIO)
import Control.Monad.Reader (MonadReader, MonadTrans, ReaderT (..), ask)
import Control.Retry (retrying)
import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import Data.Either (isLeft)
import Data.Proxy
import Hreq.Client.Internal.Config (HttpConfig (..), StatusRange (..), createDefConfig)
import Hreq.Client.Internal.HTTP (catchConnectionError, checkHttpResponse, httpResponsetoResponse,
                                  requestToHTTPRequest, runHttpClient)
import Hreq.Core.Client
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)

newtype Hreq m a = Hreq { runHreq' :: ReaderT HttpConfig m a }
  deriving (Functor, Applicative, Monad, MonadReader HttpConfig, MonadTrans, MonadIO)

type StreamConduit = forall m. MonadIO m => ConduitT () ByteString m ()

newtype ResBodyStream = ResBodyStream StreamConduit

type RunConduitClient m = RunStreamingClient m ResBodyStream

instance MonadUnliftIO m => MonadUnliftIO (Hreq m) where
  withRunInIO = wrappedWithRunInIO Hreq runHreq'

instance RunClient (Hreq IO) where
  runClient = runHttpClient

  throwHttpError = Hreq . throwM

  checkResponse = checkHttpResponse

instance RunStreamingClient (Hreq IO) ResBodyStream where
  withStreamingClient = runStreamingHttp

runHreq :: MonadIO m => BaseUrl -> Hreq m a -> m a
runHreq baseUrl action = do
  config <- liftIO $ createDefConfig baseUrl

  runHreqWithConfig config action

runHreqWithConfig :: HttpConfig -> Hreq m a -> m a
runHreqWithConfig config action = runReaderT (runHreq' action) config

runStreamingHttp
  :: forall m r. (MonadReader HttpConfig m, MonadIO m, RunClient m)
  => Request
  -> (ResBodyStream -> IO r)
  -> m r
runStreamingHttp req f = do
    config <- ask
    let manager = httpManager config
    let baseUrl = httpBaseUrl config
    let statusRange = httpStatuses config

    let httpRequest = requestToHTTPRequest baseUrl req

    let action = liftIO $ catchConnectionError $ HTTP.withResponse httpRequest manager $ \res -> do
                    checkStreamResponse res statusRange
                    f (ResBodyStream $ bodyReaderSource (HTTP.responseBody res))

    eRes <- retrying (httpRetryPolicy config) (const (return . isLeft)) (const action)
    either throwHttpError pure eRes
    where
      -- | Throws a failure error on in-correct HTTP status code.
      checkStreamResponse :: HTTP.Response HTTP.BodyReader -> StatusRange -> IO ()
      checkStreamResponse res statusRange = do
        let status = HTTP.responseStatus res
            code = statusCode status
        if code >= statusLower statusRange && code <= statusUpper statusRange
          then do
            bs <- LBS.fromChunks <$> HTTP.brConsume (HTTP.responseBody res)
            throwM $ FailureResponse req (httpResponsetoResponse (const bs) res)
          else pure ()

hreqWithConduit
  :: forall api ts v m. (HasStreamingClient api ts v m ResBodyStream)
  => HttpInput ts
  -> (StreamConduit -> IO ())
  -> m ()
hreqWithConduit input f =
  hreqStream (Proxy @api) input $ \ (ResBodyStream conduit) -> f conduit

bodyReaderSource :: MonadIO m => HTTP.BodyReader -> ConduitT i ByteString m ()
bodyReaderSource br = go
  where
    go = do
      bs <- liftIO (HTTP.brRead br)
      unless (B.null bs) $ do
        yield bs
        go
