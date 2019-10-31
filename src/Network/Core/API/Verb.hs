-- | Module for working with HTTP methods and the resulting content from
-- using them at the type level.
module Network.Core.API.Verb
  ( module Network.Core.API.Verb
    -- * Re-exports
  , StdMethod (..)
  , Method
  ) where

import Data.Proxy (Proxy)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Types.Method (Method, StdMethod (..), methodConnect, methodDelete, methodGet,
                                  methodHead, methodOptions, methodPatch, methodPost, methodPut,
                                  methodTrace)

-- | @Verb@ is a type for representing the results of an HTTP client
-- request call represented as content list and the HTTP method
-- associated with it. Type synonyms for common verbs are provided but you
-- free to define your own
data Verb (method :: k1) (contents:: [k2])
  deriving (Typeable, Generic)

type GET = 'GET
type Get = Verb GET

type POST = 'POST
type Post = Verb 'POST

type PUT = 'PUT
type Put = Verb PUT

type DELETE = 'DELETE
type Delete = Verb 'DELETE

type PATCH = 'PATCH
type Patch = Verb 'PATCH

-- | @ReflectMethod@ class provides us with a way to obtain the 'Method' data
-- associated with a promoted HTTP Verb.
class ReflectMethod a where
    reflectMethod :: Proxy a -> Method

instance ReflectMethod 'GET where
    reflectMethod _ = methodGet

instance ReflectMethod 'POST where
    reflectMethod _ = methodPost

instance ReflectMethod 'PUT where
    reflectMethod _ = methodPut

instance ReflectMethod 'DELETE where
    reflectMethod _ = methodDelete

instance ReflectMethod 'PATCH where
    reflectMethod _ = methodPatch

instance ReflectMethod 'HEAD where
    reflectMethod _ = methodHead

instance ReflectMethod 'OPTIONS where
    reflectMethod _ = methodOptions

instance ReflectMethod 'TRACE where
    reflectMethod _ = methodTrace

instance ReflectMethod 'CONNECT where
    reflectMethod _ = methodConnect
