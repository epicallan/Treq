# Hreq

[![Hackage](https://img.shields.io/hackage/v/hreq.svg?logo=haskell)](https://hackage.haskell.org/package/hreq)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build status](https://img.shields.io/travis/epicallan/hreq.svg?logo=travis)](https://travis-ci.org/epicallan/hreq)

## Intro

Hreq is a high-level easy to use type-driven HTTP client library inspired by Servant-Client. Hreq provides an alternative approach to type-safe construction and interpretation of API endpoints for Http client requests.

The Hreq github repository is a mono-repo composed of the following:

- [hreq-core](https://github.com/epicallan/hreq/tree/master/hreq-core) implementing core functionality.

- [hreq-client](https://github.com/epicallan/hreq/tree/master/hreq-client) an HTTP client using hreq-core functionality

- [hreq-conduit](https://github.com/epicallan/hreq/tree/master/hreq-conduit) an HTTP client with streaming support via conduit.

### Checkout accompanying blog post for more details

* HTTP Requests with Hreq -- TODO: add link

##  Motivation

Hreq was motivated by the simplicity and ease of use of [req](https://github.com/mrkkrp/req) and the type driven elegance of [servant-client](https://github.com/haskell-servant/servant/tree/master/servant-client).
I envisioned Hreq as the best possible compromise of both worlds.

### Some of the Key Points

 - A default HTTP client manager is set up within the library such that one doesn't have to think about manager configuration.

 - Hreq provides type synonyms for common API type combinators, therefore, reducing on API types verbosity.

 - In Hreq, API types are used directly within API functions via Type Application while in servant-client API types create new API functions for creating API requests.

 - In Hreq, API Request component arguments are provided to the API function through a Heterogeneous list.

 - Hreq supports the concept of having a Retry policy, whereby an http request is retried automatically based on a set Retry policy.

## Usage Example


```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds  #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Hreq.Client

data User = User
  { name :: String
  , age  :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

main' :: IO ()
main' = do
  res <- runHreq baseUrl $ do
    -- | Makes Post request with newUser as a request body
    createdUser <- createUser newUser
    -- | Makes Get Request with "allan" as a URL fragment
    myUser      <- getUserByName "allan"
    -- | Makes a Get Request returning a list of Users
    allUsers    <- hreq @(GetJson [User]) Empty
    return (createdUser, myUser, allUsers)
  print res
  where
    baseUrl :: BaseUrl
    baseUrl = HttpDomain "example.com"

    newUser :: User
    newUser = User "Allan" 29

createUser :: RunClient m => User -> m User
createUser user = hreq @(JsonBody User :> PostJson User) (user :. Empty)

getUserByName :: RunClient m => String -> m User
getUserByName userName = hreq @(Capture String :> GetJson User) (userName :. Empty)

```

### Attribution

Hreq is heavily inspired by [servant-client](https://github.com/haskell-servant/servant) and ideas from [Serv](https://github.com/tel/serv).

### Documentation

This README is tested by `markdown-unlit` to make sure the code builds. To keep _that_ happy, we do need a `main` in this file, so ignore the following :)

```haskell
main :: IO ()
main = pure ()
```
