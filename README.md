# Hreq

[![Hackage](https://img.shields.io/hackage/v/hreq.svg?logo=haskell)](https://hackage.haskell.org/package/hreq)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build status](https://img.shields.io/travis/epicallan/hreq.svg?logo=travis)](https://travis-ci.org/epicallan/hreq)

## Intro

Hreq is a high-level HTTP client library inspired by servant-client providing an alternative approach to type-safe construction and interpretation of API endpoints.

Hreq's API route is more Kind restricted, enabling more type correctness and more straightforward formulation of type-level functions.

##  Motivation

TODO: Write a detailed comparison between servant-client and Hreq (elaborate on the pros and cons)

Summary key points
-----------------
 - A default HTTP client manager is set up within the library such that one doesn't have to think about manager configuration. This is in stark contrast with Servant-Client where you have to provide one. It's also possible to over-ride the provided default manager

 - Hreq provides type synonyms for common API type combinators therefore reducing on API types verbosity.

 - Due to the Kind, restrictive nature of the API type specification; a wide range of invalid type errors are eliminated, making incorrect states un-representable at the type level.

 - In Hreq, API types are used directly within API functions via Type Application while in servant-client API types create new API functions for creating API requests.

 - In Servant-client valid responses must have a status code between 200 and 300. 
   In Hreq one can configure a range for valid status codes via the HTTP config with 200 to 300 as the default.
 
 - In Hreq, API Request component arguments are provided to the API function through a Heterogeneous list.

## Usage Example


```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds  #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Hreq

data User = User
  { name :: String
  , age  :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

main' :: IO ()
main' = do
  res <- runHreq baseUrl $ do
    createdUser <- createUser newUser
    myUser      <- getUserByName "allan"
    allUsers    <- hreq @(GetJson [User]) Empty
    return (createdUser, myUser, allUsers)
  print res
  where
    baseUrl :: BaseUrl
    baseUrl = BaseUrl Http "example.com" 80 "user"

    newUser :: User
    newUser = User "Allan" 29

createUser :: RunHttp m => User -> m User
createUser user = hreq @(JsonBody User :> PostJson User) (user :. Empty)

getUserByName :: RunHttp m => String -> m User
getUserByName userName = hreq @(Capture "name" String :> GetJson User) (userName  :. Empty)

```

### Attribution

Hreq borrows some code from servant-client where it makes sense to.

### Documentation

This README is tested by `markdown-unlit` to make sure the code builds. To keep _that_ happy, we do need a `main` in this file, so ignore the following :)

```haskell
main :: IO ()
main = pure ()
```

