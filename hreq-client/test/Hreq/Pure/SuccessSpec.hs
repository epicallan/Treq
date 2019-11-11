module Hreq.Pure.SuccessSpec (spec) where

import Data.Proxy
import Test.Hspec

import Data.Foldable
import Hreq.Client
import Hreq.Core.Client (RequestBody (..))
import Hreq.Pure.Util (TestState (..), TestUser (..), defaultResponse, runClientPure)

spec :: Spec
spec = describe "Hreq.SuccessSpec" successSpec

testUser :: TestUser
testUser = TestUser "Allan" 29

successSpec :: Spec
successSpec = do
  let baseUrl = HttpDomain "example.com"
      runClientPure' = runClientPure @'Default

  describe "Works with request components" $ do
    it "works with paths" $ do
       let x = hreq @("hello" :> RawResponse GET) Empty
           expected = RunClient (appendToPath "hello" defaultRequest) defaultResponse
       runClientPure' baseUrl x `shouldBe` expected

    it "works with request body" $ do
      let x = hreq @(JsonBody TestUser :> RawResponse GET) (testUser :. Empty)
          RunClient req _ = runClientPure' baseUrl x
          Just (RequestBodyBS body, _ )  = reqBody req
      body `shouldBe` mediaEncode (Proxy @JSON) testUser

    it "works with query flags" $ do
      let x = hreq @("users" :> QueryFlag "male" :> QueryFlag "old" :> RawResponse GET) Empty
          RunClient req _ = runClientPure @'Default baseUrl x
      reqPath req `shouldBe` "/users"
      toList (reqQueryString req) `shouldBe` [("male", Nothing), ("old", Nothing)]

    it "works with capture" $ do
      let x = hreq @(Capture String :> RawResponse GET) ("allan" :. Empty)
          RunClient req _ = runClientPure @'Default baseUrl x
      reqPath req `shouldBe` "/allan"

    it "works with captureAll" $ do
      let x = hreq @(CaptureAll String :> RawResponse GET) (["allan", "lukwago"] :. Empty)
          RunClient req _ = runClientPure @'Default baseUrl x
      reqPath req `shouldBe` "/allan/lukwago"

    it "works with single query params" $ do
      let x = hreq @(Param "name" String :> RawResponse GET) ("allan" :. Empty)
          RunClient req _ = runClientPure @'Default baseUrl x
      toList (reqQueryString req) `shouldBe` [("name", Just "allan")]

    it "works with multi query params" $ do
      let x = hreq @(Param "name" String :> Param "age" Int :> RawResponse GET) ("allan" :. 29 :. Empty)
          RunClient req _ = runClientPure @'Default baseUrl x
      toList (reqQueryString req) `shouldBe`  [("name", Just "allan"), ("age", Just "29")]

    it "works with multi query params as a list" $ do
      let x = hreq @(Params '["name" := String, "age" := Int] :> RawResponse GET) ("allan" :. 29 :. Empty)
          RunClient req _ = runClientPure @'Default baseUrl x
      toList (reqQueryString req) `shouldBe`  [("name", Just "allan"), ("age", Just "29")]

  describe "Works with response components" $ do
    it "works with single verb requests" $ do
      let x = hreq @(RawResponse GET) Empty
          expected = RunClient defaultRequest defaultResponse
      runClientPure' baseUrl x `shouldBe` expected

    it "works with JSON responses" $ do
       let x = hreq @(GetJson TestUser) Empty
           RunClient _ res = runClientPure @'Default baseUrl x
       res `shouldBe` testUser
