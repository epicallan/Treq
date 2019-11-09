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

setClientRequest :: Request -> ClientPure state a -> ClientPure state a
setClientRequest r h = case h of
  RunClient _ rs -> RunClient r rs
  Throw e         -> Throw e

instance RunClient (ClientPure 'Default) where
  runClient req         = RunClient req defaultResponse
  throwHttpError      = Throw
  checkResponse req _ = RunClient req Nothing

instance RunClient (ClientPure 'FailureState) where
  runClient req         = Throw $ FailureResponse req defaultResponse
  throwHttpError      = Throw
  checkResponse req _ = RunClient req Nothing

instance RunClient (ClientPure 'DecodingErrorState) where
  runClient req         = RunClient req (defaultResponse { resBody = "hey" })
  throwHttpError      = Throw
  checkResponse req _ = RunClient req Nothing

runClientPure
  :: forall state a. (Show a, Eq a)
  => BaseUrl -> ClientPure state a -> ClientPure state a
runClientPure _ httpClient = httpClient

-- Test data
data TestUser = TestUser
  { name :: String
  , age  :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
