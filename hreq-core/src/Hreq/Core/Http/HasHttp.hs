{-# LANGUAGE AllowAmbiguousTypes #-}
module Hreq.Core.Http.HasHttp where

import Control.Monad.Except
import Data.Proxy

import Hreq.Core.API
import Hreq.Core.Http.HasRequest
import Hreq.Core.Http.HasResponse
import Hreq.Core.Http.HttpError
import Hreq.Core.Http.Request
import Hreq.Core.Http.RunHttp

-- | 'HasHttp' represent constraints required to interpret a type level API into an actual
-- HTTP network request.
type HasHttp api ts v n m =
  ( ts ~ ApiToReq api      --  Turns API description into a list of Request Content types
  , v  ~ GetVerb api       --  Retrieves the verb component of an API definition
  , HasRequest ts v        --  Interprets type level list 'ReqContent' and 'Verb' components to obtain
                           --   'Request' Data
  , HasResponse v n        --  Interprets 'Verb' component to obtain type level specified Response
  , MonadError HttpError n --  @MonadError HttpError n@ is used by the 'httpRes' function
  , RunHttp m              --  Provides capability to make an actual Http client network request
  )

hreq'
  :: forall api ts v m. HasHttp api ts v (Either HttpError) m
  => Proxy api
  -> HttpInput ts
  -> m (HttpOutput v)
hreq' _ reqInput = do
  let req = httpReq (Proxy @v) (Proxy @ts) reqInput defaultRequest

  response <- runHttp $! req

  lift' $ httpRes (Proxy @v) $! response
  where
    lift' = either throwHttpError pure

-- | Used to make HTTP requests. Uses visible type-applications.
-- Example
--
-- > hreq @(Capture "age" Int :> GetJson Value) (25 :. Empty)
--
hreq
  :: forall api ts v m. HasHttp api ts v (Either HttpError) m
  => HttpInput ts
  -> m (HttpOutput v)
hreq = hreq' (Proxy @api)
