module Network.Core.API.Request where

import Data.Singletons
import GHC.TypeLits

data ReqContent a =
      Params [(Symbol, a)]
    | QueryFlags [Symbol]
    | Captures [(Symbol, a)]
    | CaptureAll Symbol a
    | ReqBody a a
    | ReqHeaders [(Symbol, a)]

type Captures = 'Captures
type QueryFlags = 'QueryFlags
type Params = 'Params
type CaptureAll = 'CaptureAll
type ReqBody = 'ReqBody
type ReqHeaders = 'ReqHeaders

data instance Sing (a :: ReqContent k) where
  SParams :: Sing ts -> Sing ('Params ts)
  SQueryFlags :: Sing ts -> Sing ('QueryFlags ts)
  SCaptures :: Sing ts -> Sing ('Captures ts)
  SCaptureAll :: Sing s -> Sing a -> Sing ('CaptureAll s a)
  SReqBody :: Sing ctyp -> Sing a -> Sing ('ReqBody ctyp a)
  SReqHeaders :: Sing a -> Sing ('ReqHeaders a)

instance SingI ts => SingI ('Params ts) where
  sing = SParams sing

instance SingI ts => SingI ('QueryFlags ts) where
  sing = SQueryFlags sing

instance SingI ts => SingI ('Captures ts) where
  sing = SCaptures sing

instance (SingI a, KnownSymbol s) => SingI ('CaptureAll s a) where
  sing = SCaptureAll sing sing

instance (SingI a, SingI ctyp) => SingI ('ReqBody ctyp a) where
  sing = SReqBody sing sing

instance SingI a => SingI ('ReqHeaders a) where
  sing = SReqHeaders sing
