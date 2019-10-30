module Network.HTTP.Hreq.Config where

import Control.Concurrent.STM.TVar (TVar)
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS

import Network.Core.API (StatusCode)
import Network.Core.Http

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

createDefConfig :: BaseUrl -> IO HttpConfig
createDefConfig baseUrl@(BaseUrl scheme _ _ _) =
  HttpConfig baseUrl (StatusRange 200 300) Nothing <$> manager
  where
    manager :: IO C.Manager
    manager = case scheme of
       Http  -> C.newManager C.defaultManagerSettings
       Https -> C.newManager TLS.tlsManagerSettings
