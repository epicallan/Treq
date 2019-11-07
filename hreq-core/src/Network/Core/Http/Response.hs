-- | This module provides the 'Response' type which is the result of an HTTP request.
--
{-# LANGUAGE DeriveFunctor #-}
module Network.Core.Http.Response where

import Data.ByteString.Lazy as LBS
import Data.Typeable
import GHC.Generics

import Network.HTTP.Types (Header, HttpVersion (..), Status)

-- * Response
data ResponseF a = Response
  { resStatus      :: Status
  , resHeaders     :: [Header]
  , resBody        :: a
  , resHttpVersion :: HttpVersion
  } deriving (Eq, Show, Generic, Typeable, Functor)

type Response = ResponseF LBS.ByteString
