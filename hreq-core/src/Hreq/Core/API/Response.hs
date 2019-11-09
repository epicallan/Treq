-- | Specification for valid Response component types used at the Kind and Type level
-- for creating an API structure.
--
module Hreq.Core.API.Response where

import Data.Kind (Type)
import Data.Singletons (Sing, SingI (..))
import GHC.TypeLits (Symbol)

-- * Response type
data ResContent a =
    ResBody a a
  | ResHeaders [(Symbol, a)]
  | ResStream a
  | Raw a

-- * Response type synonyms
type ResBody = 'ResBody
type ResHeaders = 'ResHeaders
type Raw = 'Raw ()

-- * Response as a Singleton GADT
data SResContent (a :: ResContent Type) where
  SResBody :: forall ctyp a. Sing ctyp -> Sing a -> SResContent ('ResBody ctyp a)
  SResStream :: forall a. Sing a -> SResContent ('ResStream a)
  SResHeaders :: forall (ts :: [(Symbol, Type)]). Sing ts -> SResContent ('ResHeaders ts)
  SRaw :: forall (a :: Type) . Sing a -> SResContent ('Raw a)
type instance Sing = SResContent

instance SingI a => SingI ('ResStream a :: ResContent Type) where
  sing = SResStream sing

instance (SingI ctyp, SingI a) => SingI ('ResBody ctyp a :: ResContent Type) where
  sing = SResBody sing sing

instance SingI ts => SingI ('ResHeaders ts :: ResContent Type) where
  sing = SResHeaders sing

instance SingI a => SingI ('Raw a :: ResContent Type) where
  sing = SRaw sing
