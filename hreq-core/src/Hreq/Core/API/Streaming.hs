-- | This module provides classes that one has to implement in order to
-- use a streaming library such as Conduit for streaming.
module Hreq.Core.API.Streaming where

import Data.ByteString (ByteString)
import Hreq.Core.API.Response
import Hreq.Core.API.Verb

-- * Client Streaming

-- | A StreamVerb endpoint receives a stream of encoded values at the
type StreamVerb method a = Verb method '[ 'ResStream a ]

-- * Stream synonyms
type StreamGet a = StreamVerb GET a
type StreamPost a = StreamVerb POST a
type StreamPut a = StreamVerb PUT a

-- * Request Body streaming

-- | A function which generates successive chunks of a request body,
-- provider a single empty bytestring when no more data is available.
type Pooper = IO ByteString

-- | A function which must be provided with a Popper.
type NeedsPooper a = Pooper -> IO a

-- | A datatype containing a function which will provide a 'Popper' to a 'NeedsPopper'. .
newtype GivesPooper a
  = GivesPooper { runGivesPooper ::  NeedsPooper a -> IO a }

instance Show (GivesPooper a) where
  show _ = "GivesPooper IO"

instance Eq (GivesPooper a) where
  _ == _ = False

class HasStreamBody a where
  givePopper :: a -> GivesPooper ()
