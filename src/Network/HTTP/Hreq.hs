-- | Hreq is a high-level easy to use type-driven HTTP client library inspired by Servant-Client.
-- Hreq provides an alternative approach to type-safe construction and interpretation of API
-- endpoints for Http client requests.
-- Hreq is greatly inspired by Servant Client.
--
-- == Full usage Example
--
-- > -- Test data
-- > data User = User
-- >  { name :: String
-- >  , age  :: Int
-- >  } deriving (Show, Generic, FromJSON, ToJSON)
-- >
-- > -- Program
-- > main :: IO ()
-- > main = do
-- >  res <- runHreq baseUrl $ do
-- >    -- | Make a GET request to "example.com/api/25" returning a JSON response of User type
-- >    requestedUser <-  hreq @(Capture Int :> GetJson User) (25 :. Empty)
-- >
-- >    -- | Make a POST request post to "example.com/api" with a request body of type User
-- >    -- returning a response value of type 'User'
-- >    createdUser   <-  hreq @(JsonBody User :> PostJson User) (user :. Empty)
-- >    return (requestedUser, createdUser)
-- >  print res
-- >  where
-- >    user = "Allan" 29
-- >    baseUrl = BaseUrl Http "example.com" 80 "api"
--
-- == Full usage example explanation
--
-- The first 'hreq' function takes in an API structure type argument for our case;
--
-- >>> type ApiQuery = Capture Int :> GetJson User
--
-- and an heterogeneous list containing request component values,
-- whose types are derived from the provided API structure type.
--
-- >>> (25 :. Empty) :: Hlist '[Int]
-- 25 :. Nil
--
--
-- The call to the first 'hreq' function subsequently results into the following:
--
-- @
--   'hreq' @('Capture' Int :> 'GetJson' User) (25 ':.' 'Empty')
-- @
--
-- * Safe creation of a URL endpoint of the form <http://example.com/api/25>
-- and running of an http request with that @URL@.
--
-- * Setting of the GET HTTP client call response to be of type @User@ decoded as a @JSON@ resource body.
--
-- The call to the second 'hreq' has similar results:
--
-- @
--   hreq @('JsonBody' User :> 'PostJson' User) (user :. Empty)
-- @
--
-- * Safe creation of a URL endpoint of the form <http://example.com/api/user>, setting request body to provided
-- @user@ value encoded as a JSON object and eventually running an http request with the created components.
--
-- * Setting of the POST HTTP client call response to be of type @User@ decoded as a @JSON@ resource body.
--
-- The 'runHreq' function runs the 'Hreq' monad created as a result of calling 'hreq' while also taking in a
-- 'BaseUrl' value.
--
-- ==More examples
--
-- ====Appending a path to the request path
--
-- >>> type PathsQuery = "user" :> "allan" :> GetJson User
--
-- > pathsExample :: RunHttp m => m User
--
-- >>> pathsExample = hreq @PathsQuery Empty
--
-- ====Adding query params to a request
--
-- Any type with a 'ToHttpApiData' class instance can be used as a Param value type.
--
-- >>> type SingleParam = Param "name" String :> GetJson User
--
-- > singleParamExample :: RunHttp m => m Response
--
-- >>> singleParamsExample =  hreq @SingleParam ("allan" :. Empty)
--
-- >>> type MultiParams = Param "name" String :> Param "age" Int :> GetJson User
--
-- >>> type MultiParamsList = Params '["name" := String, "age" := Int] :> GetJson User
--
-- > -- Note MultiParams and MultiParamsList are the same.
-- > -- Resulting URL is of the form http://example.com/api?name="allan"&age=20
--
-- > multiParamsExample :: RunHttp m => m User
--
-- >>> multiParamsExample = hreq @MultiParams ("allan" :. 20 :. Empty)
--
-- ====Adding QueryFlags to a request
--
-- >>> type SingleQueryFlag = "user" :> QueryFlag "male" :> GetJson User
--
-- > singleQueryFlag :: RunHttp m => m User
--
-- >>> singleQueryFlag = hreq @SingleQueryFlag Empty
--
-- >>> type MultiQueryFlags = "user" :> QueryFlag "male" :> QueryFlag "old" :> GetJson User
--
-- >>> type MultiQueryFlagList = "user" :> QueryFlags '["male", "old"] :> GetJson User
--
-- > -- Note MultiQueryFlags and MultiQueryFlagsList are the same
-- > -- The query flag values are inferred from provided type level strings (symbols)
-- > -- Resulting URL is of the form http://example.com/api?male&old
--
-- > multiFlagsExample :: RunHttp m => m User
--
-- >>> multiFlagsExample = hreq @MultiQueryFlagList Empty
--
-- ====Adding Captures
--
-- Any type with a 'ToHttpApiData' class instance can be used as a 'Capture' value type.
--
-- >>> type SingleCapture = Capture UserName :> GetJson User
--
-- >>> type MultiCapturesList = "users" :> Captures '[UserName, UserAge] :> GetJson User
-- >>> type MultiCaptures = "users" :> Capture UserName :> Capture UserAge :> GetJson User
--
-- > -- Resulting URL is of the form http://example.com/users/allan/12
-- > -- Note that MultiCapturesList is equal to MultiCaptures.
--
-- > captureExample :: RunHttp m => m User
--
-- >>> captureExample =  hreq @MultiCaptures $ UserName "allan" :. UserAge 12 :. Empty
--
-- =====CaptureAll
--
-- 'CaptureAll' is useful for a specifying a request composed of multiple URL parameter fragments of the
-- same type in a concise manner.
--
-- >>> type CaptureAllExample = "users" :> CaptureAll String :> GetJson User
--
-- > captureAllExample :: RunHttp m => m User
--
-- >>> captureAllExample = hreq @CaptureAllExample $ ["allan",  "alex", "brian"] :. Empty
--
-- ====Adding a Request body
--
-- Request bodies are created by the 'ReqBody' type. A request body type is encoded to
-- into a byteString basing on the provided media/mime type.
--
-- The library nativelysupports some media types such as 'JSON' and 'PlainText' among others.
--
-- Example type using JSON as media type, the provided body type should have an Aeson @ToJSON@ instance
--
-- >>> type ReqBodyQuery = "users" :> ReqBody User JSON :> GetJson User
--
-- The above query can be written as below:
--
-- >>> type JsonBodyQuery = "users" :> JsonBody User :> GetJson User
--
-- ==== Response type Examples
--
-- Response are represented by the @'Verb' (method :: k1) (contents:: [k2])@ type.
--
-- @method@ : is a Standard HTTP verb type such as 'GET' or 'POST'
-- @contents@ : is a type level list containing expected response from making an http call.
--
-- The library provides convenience type synonyms out of the Verb type such as @GetJson@, @PostJson@ etc.
--
-- >>> type GetPlainText a = Get '[ResBody PlainText a]
--
-- > plainTextResponse :: RunHttp m => m String
--
-- >>> plainTextResponse = hreq @("user" :> GetPlainText String) Empty
--
-- =====Returning multiple values Example
--
-- >>> type MultiResultsQuery = Get '[ ResBody JSON User, ResHeaders '[ "key-header" := String ] ]
--
-- > multiResults :: RunHttp m => m (Hlist '[ User, [Header] ])
--
-- >>> multiResults = hreq @MultiResultsQuery Empty
--
module Network.HTTP.Hreq
  ( -- * API
    module Network.Core.API
    -- * HTTP
  , module Network.Core.Http

   -- * Hreq
  , module Network.HTTP.Hreq.Internal
  , module Network.HTTP.Hreq.Config
  ) where

import Network.HTTP.Hreq.Config (HttpConfig (..), StatusRange (..), createDefConfig)
import Network.HTTP.Hreq.Internal (Hreq (..), RunHttp (..), runHreq, runHreqWithConfig)

import Network.Core.API
import Network.Core.Http

-- $setup
-- >>> import Network.Core.API
-- >>> import GHC.Generics
-- >>> import Data.Aeson
-- >>> import Data.Hlist
-- >>> data User = User deriving (Show)
-- >>> instance ToJSON User where toJSON = undefined
-- >>> instance FromJSON User where parseJSON = undefined
-- >>> newtype UserName = UserName { unUserName :: String } deriving (Show, ToHttpApiData)
-- >>> newtype UserAge = UserAge { unUserAge :: Int } deriving (Show, ToHttpApiData)
