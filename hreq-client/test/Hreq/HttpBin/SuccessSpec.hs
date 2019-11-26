module Hreq.HttpBin.SuccessSpec (spec) where

import Data.Aeson
import Hreq.Client
import Test.Hspec

spec :: Spec
spec = describe "Hreq.HttpBin.SuccessSpec" successSpec

successSpec :: Spec
successSpec = do
  let baseUrl = HttpsDomain "httpbin.org"
      runHttpBin = runHreq baseUrl

  describe "Works with HttpBin" $ do
    it "works with get requests" $ do
       r  <- runHttpBin $ hreq @("get" :> RawResponse GET) Empty
       resStatusCode r `shouldBe` 200

    it "works with specifying status codes where by received code must match expected" $ do
       r <- runHttpBin $ hreq @(Verb GET '[ ResStatus 200 ]) Empty
       resStatusCode r `shouldBe` 200

    it "works with post requests" $ do
       r  <- runHttpBin
               $ hreq @("post" :> JsonBody String :> PostJson Value) ("foo" :. Empty)

       show r `shouldContain` "foo"
