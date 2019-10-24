module Network.Core.Http.Response where

import Data.ByteString
import Network.HTTP.Types (Header, HttpVersion (..), Status)

data Response = Response
  { resStatus      :: Status
  , resHeaders     :: [Header]
  , resBody        :: ByteString
  -- ^ TODO: Turn into a type parameter so as to support streaming
  -- among other things
  , resHttpVersion :: HttpVersion
  } deriving (Show, Eq)
