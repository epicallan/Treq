module Hreq.Conduit.Internal.StreamBody
  ( ReqBodySource (..)
  ) where

import Data.ByteString
import qualified Data.ByteString as B
import Data.Conduit (ConduitT, await, ($$+), ($$++))
import Data.IORef
import Hreq.Core.API
import qualified Network.HTTP.Client as HTTP

newtype ReqBodySource = ReqBodySource (ConduitT () ByteString IO ())

instance HasStreamBody ReqBodySource where
  givePopper (ReqBodySource src)= GivesPooper $ srcToPopperIO src

-- * Helpers

-- | This is taken from "Network.HTTP.Client.Conduit" without modifications.
srcToPopperIO :: ConduitT () ByteString IO () -> HTTP.GivesPopper ()
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
