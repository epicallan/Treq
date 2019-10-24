{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeApplications #-}
module Network.Core.Http.HasResponse where

import Data.Kind
import Data.Singletons.Prelude
import Control.Monad.Except
import GHC.TypeLits
import Network.HTTP.Types (hContentType)
import Network.HTTP.Media (MediaType, parseAccept, (//), matches)

import Network.Core.API
import Network.Core.Http.Hlist
import Network.Core.Http.HttpError
import Network.Core.Http.Response

class MonadError HttpError m => HasResponse (a :: k) m where
   type HttpOutput a :: Type

   httpRes :: sing a -> Response -> m (HttpOutput a)

instance
  ( UniqMembers rs "Response"
  , HasResponse rs m
  )
  => HasResponse (Verb method rs ) m where

  type HttpOutput (Verb method rs) = HttpOutput rs

  httpRes _ = httpRes (Proxy @rs)

instance (MonadError HttpError m) => HasResponse '[] m where
  type HttpOutput '[] = ()

  httpRes _ _ = return ()

instance {-# OVERLAPPING #-} MonadError HttpError m
  => HasResponse '[ 'Raw a ] m where
  type HttpOutput '[ 'Raw a ] = Response

  httpRes _ = return

instance {-# OVERLAPPING #-}
  (MonadError HttpError m
  , TypeError ('Text "Raw response type should only be used in a singleton list")
  )
  => HasResponse ('Raw a : r : rs) m where

  type HttpOutput ('Raw a : r : rs) =
    TypeError ('Text "Raw should be used only in a singleton list")

  httpRes _ _ =  error "GHC Error"

instance {-# OVERLAPPING #-}
  ( MediaDecode ctyp a
  , MonadError HttpError m
  )
  => HasResponse '[ 'ResBody  ctyp a ] m where
  type HttpOutput '[ 'ResBody ctyp a ] = a

  httpRes _ = decodeAsBody (Proxy @ctyp)

-- | The following type instance is overly restrictive to avoid
-- overlapping type family instance error.
instance {-# OVERLAPPING #-}
  ( MediaDecode ctyp a
  , MonadError HttpError m
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
  ( MonadError HttpError m
  , SingI ('Res ('ResHeaders hs ': rs))
  , HttpResConstraints ('ResHeaders hs ': rs)
  ) => HasResponse ('ResHeaders hs ': rs) m where
  type HttpOutput ('ResHeaders hs : rs) = Hlist (HttpRes ('ResHeaders hs ': rs))

  httpRes _ response = case sing @('Res ('ResHeaders hs ': rs)) of
    SRes xs -> decodeAsHlist xs response

decodeAsBody
  :: forall ctyp a m sing . (MonadError HttpError m, MediaDecode ctyp a)
  => sing ctyp
  -> Response
  -> m a
decodeAsBody _ response = do
  responseContentType <- checkContentType

  unless (responseContentType `matches` accept)
    . throwError $ UnsupportedContentType accept response

  case decode ctypProxy (resBody response) of
     Left err -> throwError $ DecodeFailure (unDecodeError err) response
     Right val -> pure val
  where
    ctypProxy = Proxy @ctyp

    accept = mediaType ctypProxy

    checkContentType :: m MediaType
    checkContentType  = case lookup hContentType $ resHeaders response of
      Nothing -> return $ "application"//"octet-stream" -- | TODO: implement streaming
      Just t  -> maybe (throwError $ InvalidContentTypeHeader response) return $ parseAccept t

decodeAsHlist
  :: (MonadError HttpError m, HttpResConstraints rs)
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
