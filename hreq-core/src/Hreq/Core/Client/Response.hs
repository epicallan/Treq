-- | This module provides the 'Response' type which is the result of an HTTP request.
--
{-# LANGUAGE DeriveFunctor #-}
module Hreq.Core.Client.Response where

import Data.ByteString.Lazy as LBS
import Network.HTTP.Types (Header, HttpVersion (..), Status)


-- * Response
data ResponseF a = Response
  { resStatus      :: Status
  , resHeaders     :: [Header]
  , resBody        :: a
  , resHttpVersion :: HttpVersion
  } deriving (Eq, Show, Functor)

type Response = ResponseF LBS.ByteString
-- type StreamingResponse = forall a. HasStreamResponse a => ResponseF a
