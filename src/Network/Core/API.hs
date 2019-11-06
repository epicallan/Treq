-- | This module re-exports types and combinators required for type level
-- construction of API request components and expected type structure of an http request response.

module Network.Core.API
    ( -- * Request
      module Network.Core.API.Request
      -- * Response
    , module Network.Core.API.Response
      -- * API Types and Kinds
    , module Network.Core.API.Internal
      -- * MediaType
    , module Network.Core.API.MediaType
      -- * TypeLevel
    , module Network.Core.API.TypeLevel
      -- * Verb
    , module Network.Core.API.Verb
      -- * API Type Synonyms
    , module Network.Core.API
      -- * Re-exports
    , ToHttpApiData (..)
    , Header
    , Status (..)
    , HeaderName
    ) where
import Data.Kind
import Network.Core.API.Internal
import Network.Core.API.MediaType
import Network.Core.API.Request
import Network.Core.API.Response
import Network.Core.API.TypeLevel
import Network.Core.API.Verb
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
