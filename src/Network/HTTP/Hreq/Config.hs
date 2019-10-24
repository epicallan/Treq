module Network.HTTP.Hreq.Config where

import Network.Core.API (StatusCode)
import Network.Core.Http
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS

-- | Valid Response status code range
data StatusRange = StatusRange
  { statusUpper :: StatusCode
  , statusLower :: StatusCode
  }

data HttpConfig = HttpConfig
  { httpBaseUrl  :: BaseUrl
  , httpStatuses :: StatusRange
  , httpManager  :: C.Manager
  }

createDefConfig :: BaseUrl -> IO HttpConfig
createDefConfig baseUrl@(BaseUrl scheme _ _ _) =
  HttpConfig baseUrl (StatusRange 200 300) <$> manager
  where
    manager :: IO C.Manager
    manager = case scheme of
       Http  -> C.newManager C.defaultManagerSettings
       Https -> C.newManager TLS.tlsManagerSettings
