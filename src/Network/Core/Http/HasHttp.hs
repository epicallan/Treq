{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
module Network.Core.Http.HasHttp where

import Control.Monad.Except
import Data.Proxy
import Network.Core.API
import Network.Core.Http.HasRequest
import Network.Core.Http.HasResponse
import Network.Core.Http.HttpError
import Network.Core.Http.Request
import Network.Core.Http.RunHttp

type HasHttp api ts v n m =
  ( ts ~ ApiToReq api -- ^ Turns API description into a list of Request Content types
  , v  ~ GetVerb api  -- ^ Retrieves the verb component of an API definition
  , HasRequest ts v   -- ^ Interprets type level list 'ReqContent' and 'Verb' components to obtain
                      --   'Request' Data
  , HasResponse v n   -- ^ Interprets 'Verb' component to obtain type level specified Response
  , MonadError HttpError n
  , RunHttp m
  )

hreq'
  :: forall api ts v m. HasHttp api ts v (Either HttpError) m
  => Proxy api
  -> HttpInput ts
  -> m (HttpOutput v)
hreq' _ reqInput = do
  let req = httpReq (Proxy @v) (Proxy @ts) reqInput defaultRequest
  response <- runRequest $! req
  lift' $ httpRes (Proxy @v) response
  where
    lift' = either throwHttpError pure

hreq
  :: forall api ts v m. HasHttp api ts v (Either HttpError) m
  => HttpInput ts
  -> m (HttpOutput v)
hreq = hreq' (Proxy @api)

hreqWithMedia
  :: forall api ts v m. HasHttp api ts v (Either HttpError) m
  => HttpInput ts
  -> m (HttpOutput v)
hreqWithMedia = hreq' (Proxy @api)
