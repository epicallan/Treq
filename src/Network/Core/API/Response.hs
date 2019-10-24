module Network.Core.API.Response where

import Data.Singletons
import GHC.TypeLits

data ResContent a =
    ResBody a a
  | ResHeaders [(Symbol, a)]
  | Raw a -- ^ TODO: treat a as a Reponse body type parameter
          -- Such that one can get lazy or strict ByteString

type ResBody = 'ResBody
type ResHeaders = 'ResHeaders
type Raw = 'Raw ()

data instance Sing (a :: ResContent k) where
  SResBody :: Sing ctyp -> Sing a -> Sing ('ResBody ctyp a)
  SResHeaders :: Sing ts -> Sing ('ResHeaders ts)
  SRaw :: Sing a -> Sing ('Raw a)

instance (SingI ctyp, SingI a) => SingI ('ResBody ctyp a) where
  sing = SResBody sing sing

instance SingI ts => SingI ('ResHeaders ts) where
  sing = SResHeaders sing

instance SingI a => SingI ('Raw a) where
  sing = SRaw sing
