-- | This module provides classes that one has to implement in order to
-- use a streaming library such as Conduit for streaming.
module Hreq.Core.API.Streaming where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Hreq.Core.API.Response
import Hreq.Core.API.Verb

-- * Client Streaming

data Stream (a :: Type)

-- | A StreamVerb endpoint receives a stream of encoded values at the
type StreamVerb method a = Verb method '[ 'ResStream a ]

-- * Stream synonyms
type StreamGet a = StreamVerb GET a
type StreamPost a = StreamVerb POST a
type StreamPut a = StreamVerb PUT a

-- * Request Body streaming

-- | A function which has a call back that provides successive chunks of a request body.
newtype GivesPooper a
  = GivesPooper { runGivesPooper :: (IO ByteString -> IO a) -> IO a }

instance Show (GivesPooper a) where
  show _ = "GivesPooper"

instance Eq (GivesPooper a) where
  _ == _ = False

class HasStreamBody a where
  givePopper :: a -> GivesPooper ()
