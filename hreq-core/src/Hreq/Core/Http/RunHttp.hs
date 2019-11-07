-- | The 'RunHttp' module provides a class which any client must instantiate in order to
-- run HTTP requests.
--
{-# LANGUAGE DeriveFunctor #-}
module Hreq.Core.Http.RunHttp where

import Hreq.Core.Http.HttpError
import Hreq.Core.Http.Request
import Hreq.Core.Http.Response

-- | Provides the capability to run a request and get a response.
class Monad m => RunHttp m where
  runHttp :: Request -> m Response

  throwHttpError :: HttpError -> m a

  checkResponse :: Request -> Response -> m (Maybe HttpError)

-- * HttpPure

-- | A pure HTTP client monad useful for testing.
data HttpPure (state :: k) a
    = RunHttp Request a
    | Throw HttpError
  deriving (Functor, Show, Eq)

instance Monad (HttpPure state)  where
  return = pure

  m >>= f = case m of
    RunHttp r x -> setHttpRequest r $ f x
    Throw e     -> Throw e

instance Applicative (HttpPure state) where
  pure = RunHttp defaultRequest

  f <*> x = case x of
    Throw e     -> Throw e
    RunHttp r y -> setHttpRequest r $ fmap ( $ y) f

setHttpRequest :: Request -> HttpPure state a -> HttpPure state a
setHttpRequest r h = case h of
  RunHttp _ rs -> RunHttp r rs
  Throw e      -> Throw e
