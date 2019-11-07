-- | Provides Basic Authentication support
module Network.Core.Http.BasicAuth where

import Prelude ()
import Prelude.Compat

import Data.Text
import Network.Core.Http.Request

-- | Required data for Basic Authentication
data BasicAuthData = BasicAuthData
  { baUser     :: Text
  , baPassword :: Text
  }

-- | Authenticate a request using Basic Authentication
basicAuthReq :: BasicAuthData -> Request -> Request
basicAuthReq (BasicAuthData user pass) req =
  let authText = "Basic " <> user <> ":" <> pass
  in addHeader "Authorization" authText req
