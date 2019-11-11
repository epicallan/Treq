# Hreq

[![Hackage](https://img.shields.io/hackage/v/hreq-conduit.svg?logo=haskell)](https://hackage.haskell.org/package/hreq)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build status](https://img.shields.io/travis/epicallan/hreq.svg?logo=travis)](https://travis-ci.org/epicallan/hreq)

Implementation of Hreq client as an HTTP Conduit streaming client basing on hreq-core.
More streaming backends can be added in the future depending on community interest.

Please look at the repository [README.md](https://github.com/epicallan/hreq/blob/master/README.md) file for more information.

## Streaming Example

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

import Conduit
import Data.Functor (void)
import qualified Data.Text as T

import Hreq.Conduit

main' :: IO ()
main' = void $ do
  runHttpBin streamResponse
  streamFile "README.md"

runHttpBin :: Hreq IO a -> IO a
runHttpBin action = runHreq baseUrl action
  where
    baseUrl = HttpsDomain "httpbin.org"

-- | Stream data from an endpoint and write it into a temporary file
streamResponse :: RunConduitClient m => m ()
streamResponse =
  hreqWithConduit
   @("stream-bytes" :> Capture Int :> StreamGet)
    (size :. Empty)
    $ \ src -> void $ runConduitRes $ src .| sinkSystemTempFile "hreq.json"
  where
    size = 3 * 1024 * 1024 -- amount of data to stream in MBs

-- | stream data from a file and send it as a Request body stream over the network.
streamFile :: String -> IO Response
streamFile fp =
  withSourceFile fp $
     \srcFile -> do
        let src :: ReqBodySource
            src = ReqBodySource
                  $ srcFile
                  .| decodeUtf8C
                  .| mapC T.toUpper
                  .| encodeUtf8C

        runHttpBin $ streamReq src
  where
    streamReq :: RunClient m => ReqBodySource -> m Response
    streamReq src = hreq @("post" :> ConduitReqBody :> RawResponse POST) (src :. Empty)
```

### Documentation

This README is tested by `markdown-unlit` to make sure the code builds. To keep _that_ happy, we do need a `main` in this file, so ignore the following :)

```haskell
main :: IO ()
main = pure ()
```
