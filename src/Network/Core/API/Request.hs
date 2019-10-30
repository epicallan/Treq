module Network.Core.API.Request where

import Data.Kind (Type)
import Data.Singletons
import GHC.TypeLits


data ReqContent a =
      Path a Symbol
    | Params [(Symbol, a)]
    | QueryFlags a [Symbol]
    | Captures [(Symbol, a)]
    | BasicAuth a Symbol
    | CaptureAll a
    | ReqBody a a
    | ReqHeaders [(Symbol, a)]

type Captures = 'Captures
type QueryFlags = 'QueryFlags ()
type Params = 'Params
type ReqBody = 'ReqBody
type ReqHeaders = 'ReqHeaders
type CaptureAll = 'CaptureAll
type BasicAuth = 'BasicAuth ()

type Path = 'Path ()

type QueryFlag (s :: Symbol) = QueryFlags '[ s ]

type Param s t = Params '[ '( s,  t ) ]

type Capture s t = Captures '[ '( s, t) ]

data SReqContent (a :: ReqContent Type)
  = forall t s . a ~ 'Path t s => SPath (Sing t) (Sing s)
  | forall ts (b :: Type) . a ~ 'QueryFlags b ts => SQueryFlags (Sing b) (Sing ts)
  | forall ts . a ~ 'CaptureAll ts => SCaptureAll (Sing ts)
  | forall ts . a ~ 'Captures ts => SCaptures (Sing ts)
  | forall ctyp b . a ~ 'ReqBody ctyp b => SReqBody (Sing ctyp) (Sing b)
  | forall ts . a ~ ReqHeaders ts => SReqHeaders (Sing ts)
  | forall ts . a ~ Params ts => SParams (Sing ts)
  | forall t s . a ~ 'BasicAuth t s => SBasicAuth (Sing t) (Sing s)

type instance Sing = SReqContent

instance (SingI a, SingI s) => SingI ('BasicAuth a s :: ReqContent Type) where
  sing = SBasicAuth sing sing

instance (SingI a, SingI s) => SingI ('Path a s :: ReqContent Type) where
  sing = SPath sing sing

instance SingI ts => SingI ('Params ts :: ReqContent Type) where
  sing = SParams sing

instance (SingI a, SingI ts) => SingI ('QueryFlags a ts :: ReqContent Type) where
  sing = SQueryFlags sing sing

instance SingI ts => SingI ('Captures ts :: ReqContent Type) where
  sing = SCaptures sing

instance (SingI a) => SingI ('CaptureAll a :: ReqContent Type) where
  sing = SCaptureAll sing

instance (SingI a, SingI ctyp) => SingI ('ReqBody ctyp a :: ReqContent Type) where
  sing = SReqBody sing sing

instance SingI a => SingI ('ReqHeaders a :: ReqContent Type) where
  sing = SReqHeaders sing
