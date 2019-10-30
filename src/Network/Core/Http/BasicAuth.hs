module Network.Core.Http.BasicAuth where

import Prelude ()
import Prelude.Compat

import Data.Text
import Network.Core.Http.Request

data BasicAuthData = BasicAuthData
  { basicAuthUser     :: Text
  , basicAuthPassword :: Text
  }

basicAuthReq :: BasicAuthData -> Request -> Request
basicAuthReq (BasicAuthData user pass) req =
  let authText = "Basic " <> user <> ":" <> pass
  in addHeader "Authorization" authText req
