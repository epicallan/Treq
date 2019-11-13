-- | Conduit streaming support for 'Hreq'
--
module Hreq.Conduit
  ( module Hreq.Conduit.Internal.HTTP
  , module Hreq.Conduit.Internal.StreamBody
  , module Hreq.Core.API
  , module Hreq.Core.Client
  , module Hreq.Client.Internal.Config
  ) where

import Hreq.Client.Internal.Config (HttpConfig (..), StatusRange (..), createDefConfig)
import Hreq.Conduit.Internal.HTTP (Hreq (..), ResBodyStream (..), RunClient, RunConduitClient,
                                   hreqWithConduit, runHreq, runHreqWithConfig)
import Hreq.Conduit.Internal.StreamBody
import Hreq.Core.API
import Hreq.Core.Client

-- import Hreq.Client.Internal.Config (HttpConfig (..), StatusRange (..), createDefConfig)
-- import Hreq.Client.Internal.HTTP (Hreq (..), RunClient (..), runHreq, runHreqWithConfig)

  -- ( Hreq (..)
  -- , ResBodyStream (..)
  -- , RunConduitClient
  -- -- * Run Hreq client
  -- , runHreq
  -- , runHreqWithConfig
  -- , createDefConfig
  -- -- * creates Hreq Client
  -- , hreq
  -- , hreqWithConduit
