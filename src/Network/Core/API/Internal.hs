module Network.Core.API.Internal
  ( module Network.Core.API.Request
  , module Network.Core.API.Response
  , module Network.Core.API.Internal
  ) where

import Data.Kind (Type)
import Data.Singletons
import GHC.TypeLits

import Network.Core.API.Request
import Network.Core.API.Response

data Api a =
    Req [ ReqContent a]
  | Res [ ResContent a ]

data SApi (a :: Api Type)
  = forall b . a ~ 'Req b => SReq (Sing b)
  | forall b . a ~ 'Res b => SRes (Sing b)
type instance Sing = SApi

instance (SingI content) => SingI ('Res content :: Api Type) where
  sing = SRes sing

instance (SingI content) => SingI ('Req content :: Api Type) where
  sing = SReq sing

-- | API Type combinators

infixr 7 :>

data (a :: k1) :> (b :: k2)

-- | For representing type level tuples where first value is a Symbol
infixr 1 :=

type (a :: Symbol) := (b :: k2) = '( a, b)
