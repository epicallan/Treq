-- | Specification for an API endpoint as a composition of Request and Response components
-- at the Type level.
module Hreq.Core.API.Internal where

import Data.Kind (Type)
import Data.Singletons
import GHC.TypeLits

import Hreq.Core.API.Request
import Hreq.Core.API.Response

-- * API type
data Api a =
    Req [ ReqContent a]
  | Res [ ResContent a ]

-- * API GADT as a singleton.
data SApi (a :: Api Type)
  = forall b . a ~ 'Req b => SReq (Sing b)
  | forall b . a ~ 'Res b => SRes (Sing b)
type instance Sing = SApi

-- * SingI instance for API types
instance (SingI content) => SingI ('Res content :: Api Type) where
  sing = SRes sing

instance (SingI content) => SingI ('Req content :: Api Type) where
  sing = SReq sing

-- * API Type combinators
-- Used in concatenation of API types.
--
-- Examples
--
-- >>> PathQueryWithEmptyResponse = "hello" :> QueryFlag "young" :> Get '[]
-- >>> PathQueryWithResponse = "hello" :> QueryFlag "young" :> Get '[ ResBody JSON String, ResHeader [ "headerName" := String ]]
-- >>> JsonBodyAndResponse = "hello" :> JsonBody User :> GetJson User

infixr 7 :>

data (a :: k1) :> (b :: k2)

-- | For representing type level tuples where first value is a Symbol
infixr 1 :=

type (a :: Symbol) := (b :: k2) = '( a, b)

-- $setup
-- >>> import Hreq.Core.API
-- >>> import GHC.Generics
-- >>> import Data.Aeson
-- >>> data User = User deriving (Show)
-- >>> instance ToJSON User where toJSON = undefined
-- >>> instance FromJSON User where parseJSON = undefined
