module Network.Core.API.TypeLevel where

import Data.Kind
import GHC.TypeLits
import Network.Core.API.Internal
import Network.Core.API.MediaType
import Network.HTTP.Types (Header)
import Web.HttpApiData (ToHttpApiData)
import Network.Core.Http.BasicAuth (BasicAuthData)
import Network.Core.API.Verbs

type family ApiToReq (a :: k) :: [ ReqContent Type]  where
  ApiToReq (Verb m ts) =  '[ ]
  ApiToReq ( (path :: Symbol) :> ts) = 'Path () path ': ApiToReq ts
  ApiToReq ( (x :: ReqContent Type) :> ts) = x ': ApiToReq ts

type family GetVerb (a :: k) :: Type  where
  GetVerb (Verb m ts) =  Verb m ts
  GetVerb (api :> sub) = GetVerb sub

type family HttpReq (ts :: [ReqContent Type]) :: [  Type ] where
  HttpReq '[] = '[]

  HttpReq ('Path _ _ ': ts) = HttpReq ts

  HttpReq ('ReqBody ctyp a ': ts) = a ': HttpReq ts

  HttpReq ('BasicAuth _ _ : ts) = BasicAuthData ': HttpReq ts

  HttpReq ('QueryFlags _ _ ': ts) = HttpReq ts

  HttpReq ('Params ( '(s, a) ': ps ) ': ts) = a ': HttpReq ('Params ps ': ts)
  HttpReq ('Params '[] : ts) = HttpReq ts

  HttpReq ('CaptureAll a ': ts) = [a] ': HttpReq ts

  HttpReq ('Captures ( '(s, a) ': cs ) ': ts) = a ': HttpReq ('Captures cs ': ts)
  HttpReq ('Captures '[] : ts) = HttpReq ts

  HttpReq ('ReqHeaders ( '(s, a) ': hs ) ': ts) = a ': HttpReq ('ReqHeaders hs ': ts)
  HttpReq ('ReqHeaders '[] : ts) = HttpReq ts

type family HttpRes (res :: [ ResContent Type ]) :: [ Type ] where
  HttpRes '[] = '[]
  HttpRes ('ResBody ctyp a ': ts) = a ': HttpRes ts
  HttpRes ('ResHeaders (s ': hs) ': ts) = [Header] ': HttpRes ts
  HttpRes ('ResHeaders '[]  ': ts) = HttpRes ts
  HttpRes ('Raw a ': ts) = HttpRes ts

type family HttpResConstraints (res :: [ResContent Type]) :: Constraint where
  HttpResConstraints '[] = ()
  HttpResConstraints  ('ResBody ctyp a ': ts) =
     (HasMediaType ctyp, MediaDecode ctyp a, HttpResConstraints ts)
  HttpResConstraints ('ResHeaders hs ': ts) = (HttpSymbolTypePair hs, HttpResConstraints ts)
  HttpResConstraints ('Raw a ': ts) = HttpResConstraints ts

type family HttpReqConstraints (req :: [ReqContent Type]) :: Constraint where
  HttpReqConstraints '[] = ()

  HttpReqConstraints ('Path _ path ': ts) = (KnownSymbol path, HttpReqConstraints ts)

  HttpReqConstraints ('BasicAuth _ _ ': ts) = HttpReqConstraints ts

  HttpReqConstraints ('ReqBody ctyp a ': ts ) =
    (HasMediaType ctyp, MediaEncode ctyp a, HttpReqConstraints ts)

  HttpReqConstraints ('QueryFlags _a fs ': ts) = (All KnownSymbol fs, HttpReqConstraints ts)

  HttpReqConstraints ('Params ( '(s, a) ': ps) ': ts) =
    (KnownSymbol s, ToHttpApiData a,  HttpReqConstraints ('Params ps ': ts))
  HttpReqConstraints ('Params '[] ': ts) =  HttpReqConstraints ts

  HttpReqConstraints ('Captures ( '(s, a) ': cs) ': ts) =
    (KnownSymbol s, ToHttpApiData a,  HttpReqConstraints ('Captures cs ': ts))
  HttpReqConstraints ('Captures '[] ': ts) =  HttpReqConstraints ts

  HttpReqConstraints ('CaptureAll a ': ts) = (ToHttpApiData [a], HttpReqConstraints ts)

  HttpReqConstraints ('ReqHeaders ('(s, a) ': hs) ': ts) =
     (KnownSymbol s, ToHttpApiData a, HttpReqConstraints ('ReqHeaders hs ': ts))
  HttpReqConstraints ('ReqHeaders '[] ': ts) = HttpReqConstraints ts


type family HttpSymbolTypePair (ts :: [(Symbol, Type)]) :: Constraint where
  HttpSymbolTypePair ts =  (All KnownSymbol (AllFsts ts), All ToHttpApiData (AllSnds ts))

-- | Cross check that there are no repeated instance of an item
-- with in a type level list. For instance we want to have only
-- one 'ResBody' with in a Response type level list
type family UniqMembers (ts :: [k]) (label :: Symbol) :: Constraint  where
  UniqMembers '[] label = ()
  UniqMembers (a ': ts) label = (UniqMember a ts label, UniqMembers ts label)

type family UniqMember (a :: k) (ts :: [k]) (label :: Symbol) :: Constraint where
   UniqMember a '[] label = ()
   UniqMember a (a ': ts) label = TypeError ( 'Text "Type "
                                              ':<>: 'ShowType a
                                              ':<>: 'Text "Should be unique with in the "
                                              ':<>: 'Text label
                                              ':<>: 'Text " type level list"
                                             )
   UniqMember a (b ': ts ) label = UniqMember b ts label


{-------------------------------------------------------------------------------
  Helper type families
-------------------------------------------------------------------------------}

type family All (a :: k -> Constraint) (ts :: [k]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

type family AllFsts (a :: [(k1, k2)]) :: [k1] where
  AllFsts '[] = '[]
  AllFsts ('(f, s) ': ts) = f ': AllFsts ts

type family AllSnds (a :: [(k1, k2)]) :: [k2] where
  AllSnds '[] = '[]
  AllSnds ('(f, s) ': ts) = s ': AllSnds ts
