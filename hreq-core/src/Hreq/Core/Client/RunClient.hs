-- | The 'RunClient' module provides a class which any client must instantiate in order to
-- run HTTP requests.
--
{-# LANGUAGE DeriveFunctor #-}
module Hreq.Core.Client.RunClient where

import Hreq.Core.Client.ClientError
import Hreq.Core.Client.Request
import Hreq.Core.Client.Response

-- | Provides the capability to run a request and get a response.
class Monad m => RunClient m where
  runClient :: Request -> m Response

  throwHttpError :: ClientError -> m a

  checkResponse :: Request -> Response -> m (Maybe ClientError)

class Monad m => RunStreamingClient m a where
  withStreamingClient
      :: forall r. Request
      -> (a -> IO r)
      -> m r

-- * Pure client

-- | A pure HTTP client monad useful for testing.
data ClientPure (state :: k) a
    = RunClient Request a
    | Throw ClientError
  deriving (Functor, Show, Eq)

instance Monad (ClientPure state)  where
  return = pure

  m >>= f = case m of
    RunClient r x -> setHttpRequest r $ f x
    Throw e     -> Throw e

instance Applicative (ClientPure state) where
  pure = RunClient defaultRequest

  f <*> x = case x of
    Throw e     -> Throw e
    RunClient r y -> setHttpRequest r $ fmap ( $ y) f

setHttpRequest :: Request -> ClientPure state a -> ClientPure state a
setHttpRequest r h = case h of
  RunClient _ rs -> RunClient r rs
  Throw e      -> Throw e
