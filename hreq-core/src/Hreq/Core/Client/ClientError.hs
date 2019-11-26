-- | This module provides 'ClientError' constructors and type.
--
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Hreq.Core.Client.ClientError where

import Control.Exception (Exception, SomeException (..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import GHC.Generics (Generic)
import Network.HTTP.Media (MediaType)

import Hreq.Core.Client.Request
import Hreq.Core.Client.Response

-- | A type representing possible errors in a request
-- This type and the Eq instance is largely borrowed from servant-client
--
data ClientError =
  -- | The server returned an error response including the
  -- failing request. 'reqPath' includes the 'Hreq.Core.Client.BaseUrl.BaseUrl' and the
  -- path of the request.
    FailureResponse Request Response
  -- | The body could not be decoded at the expected type
  | DecodeFailure Text Response
  -- | The content-type of the response is not supported
  | UnsupportedContentType MediaType Response
  -- | The content-type header is invalid
  | InvalidContentTypeHeader Response
  -- | The received status code didn't much the expected.
  | InvalidStatusCode Response
  -- | There was a connection error, and no response was received
  | ConnectionError SomeException
  deriving (Show, Generic, Typeable)

instance Eq ClientError where
  FailureResponse req res     == FailureResponse req' res'     = req == req' && res == res'
  DecodeFailure t r           == DecodeFailure t' r'           = t == t' && r == r'
  UnsupportedContentType mt r == UnsupportedContentType mt' r' = mt == mt' && r == r'
  InvalidContentTypeHeader r  == InvalidContentTypeHeader r'   = r == r'
  ConnectionError exc         == ConnectionError exc'          = eqSomeException exc exc'
    where
      -- returns true, if type of exception is the same
      eqSomeException (SomeException a) (SomeException b) = typeOf a == typeOf b

instance Exception ClientError
