-- | Specification for valid Response component types used at the Kind and Type level
-- for creating an API structure.
--
module Hreq.Core.API.Response where

import Data.Kind (Type)
import Data.Singletons (Sing, SingI (..))
import GHC.TypeLits (Nat, Symbol)

-- * Response type
data ResContent a =
    ResBody a a
  | ResHeaders [(Symbol, a)]
  | ResStream a a
  | ResStatus a Nat
  | Raw a

-- * Response type synonyms
type ResBody = 'ResBody
type ResHeaders = 'ResHeaders
type ResStream = 'ResStream
type Raw = 'Raw ()
type ResStatus = 'ResStatus ()

-- * Response as a Singleton GADT
data SResContent (a :: ResContent Type) where
  SResBody :: forall ctyp a. Sing ctyp -> Sing a -> SResContent ('ResBody ctyp a)
  SResStream :: forall ctyp a. Sing ctyp -> Sing a -> SResContent ('ResStream ctyp a)
  SResHeaders :: forall (ts :: [(Symbol, Type)]). Sing ts -> SResContent ('ResHeaders ts)
  SResStatus :: forall (a :: Type) (n :: Nat) . Sing a -> Sing n ->  SResContent ('ResStatus a n)
  SRaw :: forall (a :: Type) . Sing a -> SResContent ('Raw a)
type instance Sing = SResContent

instance (SingI a, SingI ctyp) => SingI ('ResStream ctyp a :: ResContent Type) where
  sing = SResStream sing sing

instance (SingI ctyp, SingI a) => SingI ('ResBody ctyp a :: ResContent Type) where
  sing = SResBody sing sing

instance SingI ts => SingI ('ResHeaders ts :: ResContent Type) where
  sing = SResHeaders sing

instance (SingI a, SingI n) => SingI ('ResStatus a n :: ResContent Type) where
  sing = SResStatus sing sing

instance SingI a => SingI ('Raw a :: ResContent Type) where
  sing = SRaw sing
