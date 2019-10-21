{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeApplications #-}
module Network.Core.Http.HasResponse where

import Data.Kind
import Data.Singletons.Prelude
import GHC.TypeLits

import Network.Core.API
import Network.Core.Http.Hlist
import Network.Core.Http.HttpError
import Network.Core.Http.Response
import Network.Core.Http.RunHttp

pattern NoResponse :: Hlist '[]
pattern NoResponse = Nil

class RunHttp m => HasResponse api m where
   type HttpOutput (api :: k) :: Type

   httpRes :: sing api -> Response -> m (HttpOutput api)

instance HasResponse subroute m
  => HasResponse ((path :: Symbol) :> subroute) m where

  type  HttpOutput (path :> subroute) = HttpOutput subroute
  httpRes _ = httpRes (Proxy @subroute)

instance
  ( UniqMembers rs "Response content"
  , HasResponse rs m
  )
  => HasResponse ((ts :: [ReqContent Type]) :> Verb method rs ) m where

  type HttpOutput (ts :> Verb method rs) = HttpOutput rs

  httpRes _ = httpRes (Proxy @rs)

instance
  ( UniqMembers rs "Response"
  , HasResponse rs m
  )
  => HasResponse (Verb method rs ) m where

  type HttpOutput (Verb method rs) = HttpOutput rs

  httpRes _ = httpRes (Proxy @rs)

instance (RunHttp m) => HasResponse '[] m where
  type HttpOutput '[] = ()

  httpRes _ _ = return ()

instance {-# OVERLAPPING #-} RunHttp m
  => HasResponse '[ 'Raw a ] m where
  type HttpOutput '[ 'Raw a ] = Response

  httpRes _ = return

instance {-# OVERLAPPING #-}
  (RunHttp m
  , TypeError ('Text "Raw response type should only be used in a singleton list")
  )
  => HasResponse ('Raw : r : rs) m where

  type HttpOutput ('Raw : r : rs) =
    TypeError ('Text "Raw should be used only in a singleton list")

  httpRes _ _ =  error "GHC Error"

instance {-# OVERLAPPING #-}
  ( MediaDecode ctyp a
  , RunHttp m
  )
  => HasResponse '[ 'ResBody  ctyp a ] m where
  type HttpOutput '[ 'ResBody ctyp a ] = a

  httpRes _ = decodeAsBody (Proxy @ctyp)

-- | The following type instances below are overly restrictive to avoid
-- overlapping type family instance error.
instance {-# OVERLAPPING #-}
  ( MediaDecode ctyp a
  , RunHttp m
  , SingI ('Res (r ': rs))
  , HttpResConstraints (r ': rs)
  ) => HasResponse ( 'ResBody ctyp a ': r ': rs ) m where
  type HttpOutput  ( 'ResBody ctyp a ': r ': rs) = Hlist ( a ': HttpRes (r ': rs))
  httpRes _ response = case sing @('Res (r ': rs)) of
    SRes xs -> do
      body <- decodeAsBody (Proxy @ctyp) response
      reset <- decodeAsHlist xs response
      return $ body :. reset


instance {-# OVERLAPPING #-}
  ( RunHttp m
  , SingI ('Res ('ResHeaders hs ': rs))
  , HttpResConstraints ('ResHeaders hs ': rs)
  ) => HasResponse ('ResHeaders hs ': rs) m where
  type HttpOutput ('ResHeaders hs : rs) = Hlist (HttpRes ('ResHeaders hs ': rs))

  httpRes _ response = case sing @('Res ('ResHeaders hs ': rs)) of
    SRes xs -> decodeAsHlist xs response

decodeAsBody -- | TODO: see  decodeAs used in Servant
  :: (RunHttp m, MediaDecode ctyp a)
  => sing ctyp
  -> Response
  -> m a
decodeAsBody proxy response = case decode proxy (resBody response) of
  Left _err -> throwHttpError HttpError
  Right val -> pure val

decodeAsHlist
  :: (RunHttp m, HttpResConstraints rs)
  => Sing rs
  -> Response
  -> m (Hlist (HttpRes rs))
decodeAsHlist srs response = case srs of
  SNil -> return Nil

  SCons (SResBody ctyp _a) xs -> do
    body <- decodeAsBody ctyp response
    rest <- decodeAsHlist xs response
    return $ body :. rest

  SCons (SResHeaders (SCons _h _hs)) xs -> do
    let headers = resHeaders response
    rest <- decodeAsHlist xs response
    return $ headers :. rest

  SCons (SResHeaders SNil) xs ->
    decodeAsHlist xs response

  (SCons (SRaw _) _xs)-> error "GHC Error"
  -- ^ Should never match because we have a class instance
  -- that triggers a type error when 'Raw' is in a non-singleton
  -- type level list
