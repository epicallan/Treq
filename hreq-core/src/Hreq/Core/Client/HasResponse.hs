-- | This module provides a 'HasResponse' class that Interprets
-- 'Verb' content into a particular api endpoint query result.
--
-- For instance @Verb GET '[]@ gets interpreted as an empty response of type Unit i.e @()@
--
{-# LANGUAGE CPP #-}
module Hreq.Core.Client.HasResponse where

import Control.Monad (when, unless)
import Control.Monad.Except
import Data.Kind
import Data.Hlist
import Data.Proxy
import Data.Singletons
import qualified Data.List.NonEmpty as NE
#if MIN_VERSION_base(4,18,0)
import GHC.TypeLits hiding (withKnownNat)
#else
import GHC.TypeLits
#endif
import Network.HTTP.Types (hContentType)

import Hreq.Core.API
import Hreq.Core.Client.ClientError
import Hreq.Core.Client.Response

class MonadError ClientError m => HasResponse (a :: k) m where
   type HttpOutput a :: Type

   httpRes :: sing a -> Response -> m (HttpOutput a)

instance
  ( UniqMembers rs "Response"
  , HasResponse rs m
  )
  => HasResponse (Verb method rs ) m where

  type HttpOutput (Verb method rs) = HttpOutput rs

  httpRes _ = httpRes (Proxy @rs)

instance (MonadError ClientError m) => HasResponse '[] m where
  type HttpOutput '[] = ()

  httpRes _ _ = return ()

instance {-# OVERLAPPING #-} (MonadError ClientError m)
  => HasResponse '[ 'Raw a ] m where
  type HttpOutput '[ 'Raw a ] = Response

  httpRes _ = return

instance MonadError ClientError m => HasResponse ('ResStream ': rs) m where

  type HttpOutput ('ResStream ': rs) =
    TypeError ('Text "ResStream shouldn't be an instance of HasResponse class")

  httpRes _ _ =  error "GHC Error"

instance {-# OVERLAPPING #-}
  MonadError ClientError m
  => HasResponse ('Raw a : r : rs) m where

  type HttpOutput ('Raw a : r : rs) =
    TypeError ('Text "Raw should be used only in a singleton list")

  httpRes _ _ =  error "GHC Error"

-- | Expected status code much match received code
instance {-# OVERLAPPING #-}
  ( MonadError ClientError m
  , KnownNat n
  )
  => HasResponse '[ ResStatus n ] m where
  type HttpOutput '[ ResStatus n ] = Response

  httpRes _ res = do
    let expectedCode = fromIntegral @Integer @Int $ natVal (Proxy @n)
        rcode = resStatusCode res
    when (expectedCode /= rcode) $ throwError (InvalidStatusCode res)

    return res

-- | Expected status code much match received code in a response code list
instance {-# OVERLAPPING #-}
  ( MonadError ClientError m
  , KnownNat n
  , SingI ('Res (r ': rs))
  , HttpResConstraints (r ': rs)
  )
  => HasResponse (ResStatus n : r : rs) m where
  type HttpOutput (ResStatus n : r : rs) =  Hlist (HttpRes (r ': rs))

  httpRes _ response = do
    let expectedCode = fromIntegral @Integer @Int $ natVal (Proxy @n)
        rcode = resStatusCode response
    when (expectedCode /= rcode) $ throwError (InvalidStatusCode response)

    case sing @('Res (r ': rs)) of
      SRes xs -> decodeAsHlist xs response

instance {-# OVERLAPPING #-}
  ( MediaDecode ctyp a
  , MonadError ClientError m
  )
  => HasResponse '[ 'ResBody ctyp a ] m where
  type HttpOutput '[ 'ResBody ctyp a ] = a

  httpRes _ = decodeAsBody (Proxy @ctyp)

-- | The following type instance is overly restrictive to avoid
-- overlapping type family instance error.
instance {-# OVERLAPPING #-}
  ( MediaDecode ctyp a
  , MonadError ClientError m
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
  ( MonadError ClientError m
  , SingI ('Res ('ResHeaders hs ': rs))
  , HttpResConstraints ('ResHeaders hs ': rs)
  ) => HasResponse ('ResHeaders hs ': rs) m where
  type HttpOutput ('ResHeaders hs : rs) = Hlist (HttpRes ('ResHeaders hs ': rs))

  httpRes _ response = case sing @('Res ('ResHeaders hs ': rs)) of
    SRes xs -> decodeAsHlist xs response

decodeAsBody
  :: forall ctyp a m sing
  . (MonadError ClientError m, MediaDecode ctyp a)
  => sing ctyp
  -> Response
  -> m a
decodeAsBody _ response = do
  responseContentType <- checkContentType

  unless (any (responseContentType `matches`) accepts)
    . throwError $ UnsupportedContentType (NE.head accepts) response

  case mediaDecode ctypProxy (resBody response) of
    Left err -> throwError $ DecodeFailure (unDecodeError err) response
    Right val -> pure val
  where
    ctypProxy :: Proxy ctyp
    ctypProxy = Proxy

    accepts :: NE.NonEmpty MediaType
    accepts = mediaTypes ctypProxy

    checkContentType :: m MediaType
    checkContentType  = case lookup hContentType $ resHeaders response of
      Nothing -> return $ mediaType (Proxy @PlainText) -- fall back to plain text
      Just t  -> maybe (throwError $ InvalidContentTypeHeader response) return $ parseAccept t

-- | Turn a Response into a 'Hlist' of outputs
decodeAsHlist
  :: (MonadError ClientError m, HttpResConstraints rs)
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

  SCons (SResStatus _ snat) xs -> do
    let rcode = resStatusCode response
        expectedCode = withKnownNat snat (fromIntegral @Integer @Int $ natVal snat)

    when (rcode /= expectedCode) $ throwError (InvalidStatusCode response)

    decodeAsHlist xs response

  -- Should never match because we have a class instance
  -- that triggers a type error when 'Raw' is in a non-singleton
  -- type level list
  (SCons (SRaw _) _xs)-> error "GHC Error"

  -- Should never match because we have a class instance
  -- that triggers a type error when 'ResStream' is in a instance of
  -- HasResponse class
  (SCons (SResStream _ _) _xs)-> error "GHC Error"
