-- | This module provides functions that take API endpoint types and transform them into
-- HTTP Response outputs.
--
{-# LANGUAGE AllowAmbiguousTypes #-}
module Hreq.Core.Client.Internal where

import Control.Monad.Except
import Data.Proxy

import Hreq.Core.API
import Hreq.Core.Client.ClientError
import Hreq.Core.Client.HasRequest
import Hreq.Core.Client.HasResponse
import Hreq.Core.Client.Request
import Hreq.Core.Client.RunClient

-- | 'HasClient' represent constraints required to interpret a type level API into an actual
-- HTTP network request.
--
-- @ts ~@ 'ApiToReq' @api@ : Turns API description into a list of Request Content types
--
-- @v ~ 'GetVerb' api@     : Retrieves the verb component of an API definition
--
-- 'HasRequest' @ts v@     : Interprets type level list 'ReqContent' and 'Verb' components to obtain 'Request' Data
--
-- 'HasResponse' @v n@     : Interprets 'Verb' component to obtain type level specified Response
--
-- 'MonadError' 'ClientError' @n@ : @MonadError ClientError n@ is used by the 'httpRes' function
--
-- 'RunClient' @m@         : Provides capability to make an actual Http client network request
--
type HasClient api ts v n m =
  ( ts ~ ApiToReq api
  , v  ~ GetVerb api
  , HasRequest ts v
  , HasResponse v n
  , MonadError ClientError n
  , RunClient m
  )

-- | Used to make generic HTTP requests
--
hreq'
  :: forall api ts v m. HasClient api ts v (Either ClientError) m
  => Proxy api
  -> HttpInput ts
  -> m (HttpOutput v)
hreq' _ reqInput = do
  let req = httpReq (Proxy @v) (Proxy @ts) reqInput defaultRequest

  clientResponse <- runClient $! req

  lift' $ httpRes (Proxy @v) clientResponse
  where
    lift' = either throwHttpError pure

-- | Used to make HTTP requests. Uses visible type-applications for API type specification.
-- Example
--
-- > hreq @(Capture "age" Int :> GetJson Value) (25 :. Empty)
--
hreq
  :: forall api ts v m. HasClient api ts v (Either ClientError) m
  => HttpInput ts
  -> m (HttpOutput v)
hreq = hreq' (Proxy @api)

-- | 'HasStreamingClient' type constraint is represents constraints required for streaming HTTP responses.
--
-- @ts ~@ 'ApiToReq' @api@ :  Turns API description into a list of Request Content types
--
-- @v  ~@ 'GetVerb' @api@  :  Retrieves the verb component of an API definition
--
-- 'HasRequest' @ts v@     :  Interprets type level list 'ReqContent' and 'Verb' components to obtain
--
-- 'RunStreamingClient' @m a@: Provides capability to create an HTTP request with response streaming.
--
type HasStreamingClient api a ts v m =
  ( ts ~ ApiToReq api
  , v  ~ GetVerb api
  , HasRequest ts v
  , RunStreamingClient m a
  )

-- | Helper function for working with an HTTP response streaming client.
--
hreqStream
  :: forall api a ts v m r. (HasStreamingClient api a ts v m)
  => Proxy api
  -> HttpInput ts
  -> (a -> IO r)
  -> m r
hreqStream _ reqInput f = do
  let req = httpReq (Proxy @v) (Proxy @ts) reqInput defaultRequest

  withStreamingClient req (liftIO . f)
