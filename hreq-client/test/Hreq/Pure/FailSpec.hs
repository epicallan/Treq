module Hreq.Pure.FailSpec (spec) where

import Hreq.Client
import Hreq.Pure.Util (TestState (..), TestUser, defaultResponse, runClientPure)
import Test.Hspec

spec :: Spec
spec = describe "Hreq.FailSpec" failSpec

failSpec :: Spec
failSpec = do
  let baseUrl = HttpDomain "example.com"

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
