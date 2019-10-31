{-# LANGUAGE DeriveFunctor #-}
module Network.Core.Http.Response where

import Data.ByteString.Lazy as LBS
import Data.Typeable
import GHC.Generics

import Network.HTTP.Types (Header, HttpVersion (..), Status)

data ResponseF a = Response
  { resStatus      :: Status
  , resHeaders     :: [Header]
  , resBody        :: a
  -- ^ TODO: Turn into a type parameter so as to support streaming
  -- among other things
  , resHttpVersion :: HttpVersion
  } deriving (Eq, Show, Generic, Typeable, Functor)

type Response = ResponseF LBS.ByteString
