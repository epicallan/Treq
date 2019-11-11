module Hreq.Conduit.Internal.StreamBody where

import Data.ByteString
import qualified Data.ByteString as B
import Data.Conduit (ConduitT, await, ($$+), ($$++))
import Data.IORef
import Hreq.Core.API
import qualified Network.HTTP.Client as HTTP

-- | The conduit type representing a streaming request body.
type BodyConduit = ConduitT () ByteString IO ()

-- | The Request body Stream is treated as a chucked body stream 'HTTP.RequestBodyStreamChunked'.
-- Ensure your server supports chucked body stream.
newtype ReqBodySource = ReqBodySource BodyConduit

-- | For use in API endpoint type definition
-- >>> type ExampleQuery = "post" :> ConduitReqBody :> RawResponse POST
--
type ConduitReqBody = StreamBody OctetStream ReqBodySource

instance HasStreamBody ReqBodySource where
  givePopper (ReqBodySource src)= GivesPooper $ srcToPopperIO src

-- * Helpers

-- | This is taken from "Network.HTTP.Client.Conduit" without modifications.
srcToPopperIO :: BodyConduit -> HTTP.GivesPopper ()
srcToPopperIO src f = do
  (rsrc0, ()) <- src $$+ return ()
  irsrc <- newIORef rsrc0
  let popper :: IO ByteString
      popper = do
        rsrc <- readIORef irsrc
        (rsrc', mres) <- rsrc $$++ await
        writeIORef irsrc rsrc'
        case mres of
          Nothing -> return B.empty
          Just bs
              | B.null bs -> popper
              | otherwise -> return bs
  f popper

-- $setup
-- >>> import Hreq.Core.API
-- >>> import Hreq.Conduit.Internal.StreamBody
