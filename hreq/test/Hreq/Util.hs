{-# LANGUAGE DeriveAnyClass #-}
module Hreq.Util where

import Data.Aeson
import GHC.Generics

import Hreq.Client
import Network.HTTP.Types (hContentType, http11, status200)

defaultResponse :: Response
defaultResponse = Response
 { resStatus = status200
 , resHeaders = [(hContentType, "application/json")]
 , resHttpVersion = http11
 , resBody = "{\"name\":\"Allan\",\"age\":29}"
 }

data TestState =
    FailureState
  | DecodingErrorState
  | Default

setClientRequest :: Request -> HttpPure state a -> HttpPure state a
setClientRequest r h = case h of
  RunHttp _ rs -> RunHttp r rs
  Throw e         -> Throw e

instance RunHttp (HttpPure 'Default) where
  runHttp req         = RunHttp req defaultResponse
  throwHttpError      = Throw
  checkResponse req _ = RunHttp req Nothing

instance RunHttp (HttpPure 'FailureState) where
  runHttp req         = Throw $ FailureResponse req defaultResponse
  throwHttpError      = Throw
  checkResponse req _ = RunHttp req Nothing

instance RunHttp (HttpPure 'DecodingErrorState) where
  runHttp req         = RunHttp req (defaultResponse { resBody = "hey" })
  throwHttpError      = Throw
  checkResponse req _ = RunHttp req Nothing

runHttpPure
  :: forall state a. (Show a, Eq a)
  => BaseUrl -> HttpPure state a -> HttpPure state a
runHttpPure _ httpClient = httpClient

-- Test data
data TestUser = TestUser
  { name :: String
  , age  :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
