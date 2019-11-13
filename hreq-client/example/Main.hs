{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hreq.Client

data User = User
  { name :: Text
  , age  :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Assume we are making requests against an HTTP service providing a JSON user management API
--
baseUrl :: BaseUrl
baseUrl = HttpUrl "example.com" "user"

getUserByName :: RunClient m => Text -> m User
getUserByName userName = hreq @(Capture Text :> GetJson User) (userName :. Empty)

getAllUsers :: RunClient m => m [User]
getAllUsers = hreq @("all" :> GetJson [User]) Empty

createUser :: RunClient m => User -> m ()
createUser user = hreq @(JsonBody User :> EmptyResponse POST) (user :. Empty)

-- | Don't run main without supplying a functioning baseUrl.
main :: IO ()
main = runHreq baseUrl $ do
  reqUser     <- getUserByName "allan"
  createdUser <- createUser newUser
  allUsers    <- getAllUsers
  -- Delete users with age equal to 20
  hreq @(Capture Int :> EmptyResponse DELETE) (20 :. Empty)
  -- do something with API data
  liftIO $ print (reqUser, createdUser, allUsers)
  where
    newUser :: User
    newUser = User "allan" 12
