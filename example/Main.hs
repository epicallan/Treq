{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, Value)
import GHC.Generics (Generic)
import Network.HTTP.Hreq

data User = User
  { name :: String
  , age  :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

main :: IO ()
main = do
  print baseUrl
  res <- runHreq baseUrl $ do
    x <-  hreq @(GetJSON Value) Empty
    y <-  hreq @(RawResponse GET) Empty
    r <-  hreq @("hello" :> RawResponse GET) Empty
    return (x, y, r)
  print res
  where
    baseUrl = BaseUrl Http "trequest.free.beeceptor.com" 80 ""

user :: User
user = User "Allan" 29

{-------------------------------------------------------------------------------
  Simple Query examples
-------------------------------------------------------------------------------}

singleQueryFlag :: RunHttp m => m Value
singleQueryFlag = hreq @(QueryFlag "age" :> GetJSON Value) Empty

singleParam :: RunHttp m => m Value
singleParam = hreq @(Param "age" Int :> GetJSON Value) $ singleton 10

singleCapture :: RunHttp m => m Value
singleCapture = hreq @(Capture "age" Int :> GetJSON Value) (25 :. Empty)

singleReqJsonBody :: RunHttp m => m User
singleReqJsonBody = hreq @(JSONBody User :> PostJSON User) (user :. Empty)

-- | Generic use of ReqBody type with any valid content type
singleReqBody :: RunHttp m => m User
singleReqBody = hreq @('[ ReqBody PlainText User ] :> PostJSON User) (user :. Empty)

withNoRequestComponent :: RunHttp m => m Value
withNoRequestComponent = hreq @(GetJSON Value) Empty

emptyResponse :: RunHttp m => m ()
emptyResponse = hreq @(EmptyResponse GET) Empty

rawResponse :: RunHttp m => m Response
rawResponse = hreq @(RawResponse GET) Empty


{-------------------------------------------------------------------------------
  Multi-Request & Multi Response component examples
-------------------------------------------------------------------------------}

type Query =
  '[ Params '[ "age" := Int, "name" := String], QueryFlags '[ "teacher", "new"] ]
  :> GetJSON User

ex6 :: RunHttp m => m User
ex6 = hreq @Query (1 :. "Allan" :. Empty)

type Query1 =
   '[ Params '[ "age" := Int, "name" := String] ]
  :>  Get '[ ResBody JSON User, ResHeaders '[ "some-header-name" := String ] ]

ex7 :: RunHttp m => m (Hlist '[ User, [Header] ])
ex7 = hreq @Query1  (1 :. "Allan" :. Empty)
