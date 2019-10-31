-- | Specification for valid Response component types used at the Kind and Type level
-- for creating an API structure.
--
module Network.Core.API.Response where

import Data.Kind (Type)
import Data.Singletons (Sing, SingI (..))
import GHC.TypeLits (Symbol)

-- * Response type
data ResContent a =
    ResBody a a
  | ResHeaders [(Symbol, a)]
  | Raw a

-- * Response type synonyms
type ResBody = 'ResBody
type ResHeaders = 'ResHeaders
type Raw = 'Raw ()

-- * Response as a Singleton GADT
data SResContent (a :: ResContent Type) where
  SResBody :: forall ctyp a. Sing ctyp -> Sing a -> SResContent ('ResBody ctyp a)
  SResHeaders :: forall (ts :: [(Symbol, Type)]). Sing ts -> SResContent ('ResHeaders ts)
  SRaw :: forall (a :: Type) . Sing a -> SResContent ('Raw a)
type instance Sing = SResContent

-- * SingI instances
instance (SingI ctyp, SingI a) => SingI ('ResBody ctyp a :: ResContent Type) where
  sing = SResBody sing sing

instance SingI ts => SingI ('ResHeaders ts :: ResContent Type) where
  sing = SResHeaders sing

instance SingI a => SingI ('Raw a :: ResContent Type) where
  sing = SRaw sing
