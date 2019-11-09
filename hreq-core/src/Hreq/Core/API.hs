-- | This module re-exports types and combinators required for type level
-- construction of API request components and expected type structure of an http request response.

module Hreq.Core.API
    ( -- * Request
      module Hreq.Core.API.Request
      -- * Response
    , module Hreq.Core.API.Response
      -- * API Types and Kinds
    , module Hreq.Core.API.Internal
      -- * MediaType
    , module Hreq.Core.API.MediaType
       -- * Streaming
    , module Hreq.Core.API.Streaming
      -- * TypeLevel
    , module Hreq.Core.API.TypeLevel
      -- * Verb
    , module Hreq.Core.API.Verb
      -- * API Type Synonyms
    , module Hreq.Core.API
      -- * Re-exports
    , ToHttpApiData (..)
    , Header
    , Status (..)
    , HeaderName
    ) where
import Data.Kind
import Hreq.Core.API.Internal
import Hreq.Core.API.MediaType
import Hreq.Core.API.Request
import Hreq.Core.API.Response
import Hreq.Core.API.Streaming
import Hreq.Core.API.TypeLevel
import Hreq.Core.API.Verb
import Network.HTTP.Types (Header, HeaderName, Status (..))
import Web.HttpApiData (ToHttpApiData (..))

type StatusCode = Int

type JsonBody (a :: Type) = ReqBody JSON a

-- | ==Response Type synonyms

type GetJson a = Get '[ ResBody JSON a]

type PostJson a = Post '[ResBody JSON a]

type PutJson a = Put '[ ResBody JSON a]

type PatchJson a = Patch '[ ResBody JSON a]

type DeleteJson a = Delete '[ResBody JSON a]

type RawResponse v = Verb v '[ Raw ]

type EmptyResponse v = Verb v ('[ ] :: [ ResContent Type ])
