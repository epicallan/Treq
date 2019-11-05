{-# LANGUAGE TypeApplications #-}
module Hreq.SuccessSpec (spec) where

import Data.Proxy
import Test.Hspec

import Hreq.Util (TestState (..), TestUser (..), defaultResponse, runHttpPure)
import Network.HTTP.Hreq

spec :: Spec
spec = describe "Hreq.SuccessSpec" successSpec

testUser :: TestUser
testUser = TestUser "Allan" 29

successSpec :: Spec
successSpec = do
  let baseUrl = BaseUrl Http "example.com" 80 ""
      runHttpPure' = runHttpPure @'Default

  describe "Works with request components" $ do
    it "works with paths" $ do
       let x = hreq @("hello" :> RawResponse GET) Empty
           expected = RunHttp (appendToPath "hello" defaultRequest) defaultResponse
       runHttpPure' baseUrl x `shouldBe` expected

    it "works with request body" $ do
      let x = hreq @(JsonBody TestUser :> RawResponse GET) (testUser :. Empty)
          RunHttp req _ = runHttpPure' baseUrl x
          Just (body, _ )  = reqBody req
      body `shouldBe` mediaEncode (Proxy @JSON) testUser

    it "works with capture" $ do
      let x = hreq @(Capture String :> RawResponse GET) ("allan" :. Empty)
          RunHttp req _ = runHttpPure @'Default baseUrl x
      reqPath req `shouldBe` "allan"

  describe "Works with response components" $ do
    it "works with single verb requests" $ do
      let x = hreq @(RawResponse GET) Empty
          expected = RunHttp defaultRequest defaultResponse
      runHttpPure' baseUrl x `shouldBe` expected

    it "works with JSON responses" $ do
       let x = hreq @(GetJson TestUser) Empty
           RunHttp _ res = runHttpPure @'Default baseUrl x
       res `shouldBe` testUser
