-- | Hreq offers an alternative type driven approach to construction and creation of HTTP client
-- requests.
-- One of its goals is to offer better user experience than the current solutions by leveraging
-- Type level programming.
--
-- Hreq is greatly inspired by Servant Client. See the Readme file for difference between Hreq and servant-client.
--
-- == Example
-- > -- Test data
-- > data User = User
-- >  { name :: String
-- >  , age  :: Int
-- >  } deriving (Show, Generic, FromJSON, ToJSON)
-- >
-- > -- Program
-- > main :: IO ()
-- > main = do
-- >  res <- runHreq baseUrl $ do
-- >    requestedUser <-  hreq @(Capture Int :> GetJson User) (25 :. Empty)
-- >    createdUser   <-  hreq @(JsonBody User :> PostJson User) (user :. Empty)
-- >    return (requestedUser, createdUser)
-- >  print res
-- >  where
-- >    user = "Allan" 29
-- >    baseUrl = BaseUrl Http "example.com" 80 "hello"
--
module Network.HTTP.Hreq
  ( -- * API
    module Network.Core.API
    -- * HTTP
  , module Network.Core.Http

   -- * Hreq
  , module Network.HTTP.Hreq.Internal
  , module Network.HTTP.Hreq.Config
  ) where

import Network.HTTP.Hreq.Config (HttpConfig (..), StatusRange (..), createDefConfig)
import Network.HTTP.Hreq.Internal (Hreq (..), runHreq, runHreqWithConfig)

import Network.Core.API
import Network.Core.Http
