{-# LANGUAGE AllowAmbiguousTypes #-}
module Hreq.Core.Client.HasClient where

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
-- Using type constraints (~) with in the @HasClient@ type synonym to have constraintss like @HasRequest ts v@
-- is a hack to circumvent the fact that GHC doesn't work with type families in instance heads.
--
type HasClient api ts v n m =
  ( ts ~ ApiToReq api        --  Turns API description into a list of Request Content types
  , v  ~ GetVerb api         --  Retrieves the verb component of an API definition
  , HasRequest ts v          --  Interprets type level list 'ReqContent' and 'Verb' components to obtain
                             --   'Request' Data
  , HasResponse v n          --  Interprets 'Verb' component to obtain type level specified Response
  , MonadError ClientError n --  @MonadError ClientError n@ is used by the 'httpRes' function
  , RunClient m              --  Provides capability to make an actual Http client network request
  )


hreq'
  :: forall api ts v m. HasClient api ts v (Either ClientError) m
  => Proxy api
  -> HttpInput ts
  -> m (HttpOutput v)
hreq' _ reqInput = do
  let req = httpReq (Proxy @v) (Proxy @ts) reqInput defaultRequest

  clientResponse <- runClient req

  lift' $ httpRes (Proxy @v) $! clientResponse
  where
    lift' = either throwHttpError pure

-- | Used to make HTTP requests. Uses visible type-applications.
-- Example
--
-- > hreq @(Capture "age" Int :> GetJson Value) (25 :. Empty)
--
hreq
  :: forall api ts v m. HasClient api ts v (Either ClientError) m
  => HttpInput ts
  -> m (HttpOutput v)
hreq = hreq' (Proxy @api)

-- | TODO: possibly get rid of this type synonym
type HasStreamingClient api ts v m a =
  ( ts ~ ApiToReq api      --  Turns API description into a list of Request Content types
  , v  ~ GetVerb api       --  Retrieves the verb component of an API definition
  , HasRequest ts v        --  Interprets type level list 'ReqContent' and 'Verb' components to obtain
                           --   'Request' Data
  , RunStreamingClient m a
  )

hreqStream
  :: forall api a r ts v m. (HasStreamingClient api ts v m a)
  => Proxy api
  -> HttpInput ts
  -> (a -> IO r)
  -> m r
hreqStream _ reqInput f = do
  let req = httpReq (Proxy @v) (Proxy @ts) reqInput defaultRequest

  withStreamingClient req (liftIO . f)