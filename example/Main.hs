{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Hreq

data User = User
  { name :: String
  , age  :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

main :: IO ()
main = do
  res <- runHreq baseUrl $ do
    x <- singleQueryFlag
    y <- singleReqBody
    return (y, x)
  print res
  where
    baseUrl = BaseUrl Http "trequest.free.beeceptor.com" 80 ""

user :: User
user = User "Allan" 29

{-------------------------------------------------------------------------------
  Simple Query examples
-------------------------------------------------------------------------------}

singleQueryFlag :: RunHttp m => m Value
singleQueryFlag = hreq @("user" :? QueryFlag "age" :> GetJSON Value) EmptyReq

singleParam :: RunHttp m => m Value
singleParam = hreq @(Param "age" Int :> GetJSON Value) (1 :. EmptyReq)

singleCapture :: RunHttp m => m Value
singleCapture = hreq @(Capture "age" Int :> GetJSON Value) (25 :. EmptyReq)

singleReqJsonBody :: RunHttp m => m User
singleReqJsonBody = hreq @(JSONBody User :> PostJSON User) (user :. EmptyReq)

-- | Generic use of ReqBody type with any valid content type
singleReqBody :: RunHttp m => m User
singleReqBody = hreq @('[ ReqBody PlainText User ] :> PostJSON User) (user :. EmptyReq)

withNoRequestComponent :: RunHttp m => m Value
withNoRequestComponent = hreq @(GetJSON Value) EmptyReq

emptyResponse :: RunHttp m => m ()
emptyResponse = hreq @(EmptyResponse GET) EmptyReq

rawResponse :: RunHttp m => m Response
rawResponse = hreq @(RawResponse GET) EmptyReq

{-------------------------------------------------------------------------------
  Multi-Request & Multi Response component examples
-------------------------------------------------------------------------------}

type Query =
  '[ Params '[ "age" := Int, "name" := String], QueryFlags '[ "teacher", "new"] ]
  :> GetJSON User

ex6 :: RunHttp m => m User
ex6 = hreq @Query (1 :. "Allan" :. EmptyReq)

type Query1 =
   '[ Params '[ "age" := Int, "name" := String] ]
  :>  Get '[ ResBody JSON User, ResHeaders '[ "some-header-name" := String ] ]

ex7 :: RunHttp m => m (Hlist '[ User, [Header] ])
ex7 = hreq @Query1  (1 :. "Allan" :. EmptyReq)
