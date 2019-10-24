module Network.Core.API.Request where

import Data.Singletons
import GHC.TypeLits


data ReqContent a =
      Path a Symbol -- ^ a is not used, its here for type checking
    | Params [(Symbol, a)]
    | QueryFlags a [Symbol]
    -- ^ We don't really need the a, its here to appease type checking gods
    | Captures [(Symbol, a)]
    | CaptureAll a
    | ReqBody a a
    | ReqHeaders [(Symbol, a)]

type Captures = 'Captures
type QueryFlags = 'QueryFlags ()
type Params = 'Params
type ReqBody = 'ReqBody
type ReqHeaders = 'ReqHeaders
type CaptureAll = 'CaptureAll

type Path = 'Path ()

type QueryFlag (s :: Symbol) = QueryFlags '[ s ]

type Param s t = Params '[ '( s,  t ) ]

type Capture s t = Captures '[ '( s, t) ]

data instance Sing (a :: ReqContent k) where
  SPath :: Sing a -> Sing s -> Sing ('Path a s)
  SParams :: Sing ts -> Sing ('Params ts)
  SQueryFlags :: Sing a -> Sing ts -> Sing ('QueryFlags a ts)
  SCaptures :: Sing ts -> Sing ('Captures ts)
  SCaptureAll :: Sing a -> Sing (CaptureAll a)
  SReqBody :: Sing ctyp -> Sing a -> Sing ('ReqBody ctyp a)
  SReqHeaders :: Sing a -> Sing ('ReqHeaders a)

instance (SingI a, SingI s) => SingI ('Path a s) where
  sing = SPath sing sing

instance SingI ts => SingI ('Params ts) where
  sing = SParams sing

instance (SingI a, SingI ts) => SingI ('QueryFlags a ts) where
  sing = SQueryFlags sing sing

instance SingI ts => SingI ('Captures ts) where
  sing = SCaptures sing

instance (SingI a) => SingI ('CaptureAll a) where
  sing = SCaptureAll sing

instance (SingI a, SingI ctyp) => SingI ('ReqBody ctyp a) where
  sing = SReqBody sing sing

instance SingI a => SingI ('ReqHeaders a) where
  sing = SReqHeaders sing
