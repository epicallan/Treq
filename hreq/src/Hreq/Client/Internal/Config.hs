-- | 'HttpConfig' is used in the 'Hreq.Client.Internal.HTTP.Hreq' Monad for HTTP client configuration
--
module Hreq.Client.Internal.Config
  ( -- * HttpConfig
    HttpConfig (..)
    -- * Status Range
  , StatusRange (..)
    -- * Helper function
  , createDefConfig
  ) where

import Control.Concurrent.STM.TVar (TVar)
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS

import Hreq.Core.API (StatusCode)
import Hreq.Core.Client

-- | Valid Response status code range
data StatusRange = StatusRange
  { statusUpper :: StatusCode
  , statusLower :: StatusCode
  }

data HttpConfig = HttpConfig
  { httpBaseUrl   :: BaseUrl
  , httpStatuses  :: StatusRange
  , httpCookieJar :: Maybe (TVar C.CookieJar)
  , httpManager   :: C.Manager
  }

-- | Function for creating a default 'HttpConfig'
createDefConfig :: BaseUrl -> IO HttpConfig
createDefConfig baseUrl@(BaseUrl scheme _ _ _) =
  HttpConfig baseUrl (StatusRange 200 300) Nothing <$> manager
  where
    manager :: IO C.Manager
    manager = case scheme of
       Http  -> C.newManager C.defaultManagerSettings
       Https -> C.newManager TLS.tlsManagerSettings
