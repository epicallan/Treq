module Hreq.FailSpec (spec) where

import Hreq.Util (TestState (..), TestUser, defaultResponse, runClientPure)
import Hreq.Client
import Test.Hspec

spec :: Spec
spec = describe "Hreq.FailSpec" failSpec

failSpec :: Spec
failSpec = do
  let baseUrl = BaseUrl Http "example.com" 80 ""

  describe "throw appropriate errors" $ do
    it "throw failure error" $ do
      let x = hreq @("hello" :> RawResponse GET) Empty
          req' = appendToPath "hello" defaultRequest
      runClientPure @'FailureState baseUrl x
        `shouldBe` Throw (FailureResponse req' defaultResponse)

    it "throws decoding failure error" $ do
      let x = hreq @(GetJson TestUser) Empty

      show (runClientPure @'DecodingErrorState baseUrl x)
        `shouldContain` "DecodeFailure"
