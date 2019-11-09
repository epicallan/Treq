{-# LANGUAGE AllowAmbiguousTypes #-}
module Hreq.Conduit.Internal.HTTP
  ( Hreq (..)
  , ResBodyStream (..)
  , RunConduitClient
  , hreq
  , hreqConduit
  ) where

import Prelude ()
import Prelude.Compat

import Control.Monad
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..), wrappedWithRunInIO)
import Control.Monad.Reader (MonadReader, MonadTrans, ReaderT (..), ask)
import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import Data.Proxy
import Hreq.Client.Internal.Config (HttpConfig (..), StatusRange (..))
import Hreq.Client.Internal.HTTP (checkHttpResponse, httpResponsetoResponse, requestToHTTPRequest,
                                  runHttpClient)
import Hreq.Core.Client
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)

newtype Hreq m a = Hreq { runHreq' :: ReaderT HttpConfig m a }
  deriving (Functor, Applicative, Monad, MonadReader HttpConfig, MonadTrans, MonadIO)

newtype ResBodyStream = ResBodyStream (ConduitT () ByteString IO ())

type RunConduitClient m = RunStreamingClient m ResBodyStream

instance MonadUnliftIO m => MonadUnliftIO (Hreq m) where
  withRunInIO = wrappedWithRunInIO Hreq runHreq'

instance RunClient (Hreq IO) where
  runClient = runHttpClient

  throwHttpError err = Hreq $ throwM err >>= return

  checkResponse = checkHttpResponse

instance RunStreamingClient (Hreq IO) ResBodyStream where
  withStreamingClient req f = do
    config <- ask

    let manager = httpManager config
    let baseUrl = httpBaseUrl config
    let statusRange = httpStatuses config

    let httpRequest = requestToHTTPRequest baseUrl req

    liftIO $ HTTP.withResponse httpRequest manager $ \res -> do
      let status = HTTP.responseStatus res
          code = statusCode status


      unless (code >= statusLower statusRange && code <= statusUpper statusRange) $ do
        bs <- LBS.fromChunks <$> HTTP.brConsume (HTTP.responseBody res)
        throwM  $ FailureResponse req (httpResponsetoResponse (const bs) res)

      f $ ResBodyStream $ bodyReaderSource (HTTP.responseBody res)

hreqConduit
  :: forall api ts v m. (HasStreamingClient api ts v m ResBodyStream)
  => HttpInput ts
  -> (ConduitT () ByteString IO () -> IO ())
  -> m ()
hreqConduit input f =
  hreqStream (Proxy @api) input $ \ (ResBodyStream conduit) -> f conduit

--  Helper functions
---------------------

bodyReaderSource :: MonadIO m => HTTP.BodyReader -> ConduitT i ByteString m ()
bodyReaderSource br = go
  where
    go = do
      bs <- liftIO (HTTP.brRead br)
      unless (B.null bs) $ do
        yield bs
        go
