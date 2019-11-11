module Hreq.HttpBin.SuccessSpec (spec) where

import Conduit
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Hreq.Conduit
import Test.Hspec

spec :: Spec
spec = describe "Hreq.HttpBin.SuccessSpec" successSpec

runHttpBin :: Hreq IO a -> IO a
runHttpBin action = runHreq baseUrl action
  where
    baseUrl = HttpsDomain "httpbin.org"

successSpec :: Spec
successSpec = do
  describe "Streaming" $ do
    describe "works with responses" $ do
      it "receiving a stream of data" $ do
        let size = 3 * 1024 * 1024 -- amount of data to stream in MBs
        let streamBytes = hreqWithConduit @("stream-bytes" :> Capture Int :> StreamGet) (size :. Empty)

        runHttpBin
          $ streamBytes
          $ \ src -> do
                tempFile <- runConduitRes $ src .| sinkSystemTempFile "hreq.json"
                tempFile `shouldContain` "hreq"


    describe "works with request bodies" $ do
      it "streaming a file " $ do
        let streamReq src = hreq @("post" :> ConduitReqBody :> RawResponse POST) $ src :. Empty

        withSourceFile "README.md" $
           \srcFile -> do
              let src :: ReqBodySource
                  src = ReqBodySource
                        $ srcFile
                        .| decodeUtf8C
                        .| mapC T.toUpper
                        .| encodeUtf8C

              res <- runHttpBin $ streamReq src

              cs (resBody res) `shouldContain` "HREQ"
