{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

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
  res <- runHreq baseUrl $ do
    x <-  hreq @(GetJson Value) Empty
    y <-  hreq @(RawResponse GET) Empty
    return (x, y)
  print res
  where
    baseUrl = BaseUrl Http "trequest.free.beeceptor.com" 80 "hello"

user :: User
user = User "Allan" 29

{-------------------------------------------------------------------------------
  Simple Query examples
-------------------------------------------------------------------------------}

singleQueryFlag :: RunHttp m => m Value
singleQueryFlag = hreq @(QueryFlag "age" :> GetJson Value) Empty

singleParam :: RunHttp m => m Value
singleParam = hreq @(Param "age" Int :> GetJson Value) $ singleton 10

singleCapture :: RunHttp m => m Value
singleCapture = hreq @(Capture Int :> GetJson Value) (25 :. Empty)

singleReqJsonBody :: RunHttp m => m User
singleReqJsonBody = hreq @(JsonBody User :> PostJson User) (user :. Empty)

-- | Generic use of ReqBody type with any valid content type
singleReqBody :: RunHttp m => m User
singleReqBody = hreq @(ReqBody PlainText User :> PostJson User) (user :. Empty)

withNoRequestComponent :: RunHttp m => m Value
withNoRequestComponent = hreq @(GetJson Value) Empty

emptyResponse :: RunHttp m => m ()
emptyResponse = hreq @(EmptyResponse GET) Empty

rawResponse :: RunHttp m => m Response
rawResponse = hreq @(RawResponse GET) Empty


{-------------------------------------------------------------------------------
  Multi-Request & Multi Response component examples
-------------------------------------------------------------------------------}

-- | You can use type level lists for multiple value listing in Flags and Params
type Query =
  Params '[ "age" := Int, "height" := Int] :> QueryFlags '[ "teacher", "new"] :> GetJson User

ex1 :: RunHttp m => m User
ex1 = hreq @Query (20 :. 5 :.  Empty)

-- | You can also use singular combinators to represent multiple values
type Query1 =
  Param "age" Int :> Param "height" Int :> QueryFlag "teacher" :> QueryFlag "new" :> GetJson User

ex2 :: RunHttp m => m User
ex2 = hreq @Query1 (20 :. 5 :. Empty)

type Query2 =
     Params '[ "name" := String ]
  :> Get '[ ResBody JSON User, ResHeaders '[ "some-header-name" := String ] ]

ex7 :: RunHttp m => m (Hlist '[ User, [Header] ])
ex7 = hreq @Query2  ("Allan" :. Empty)
