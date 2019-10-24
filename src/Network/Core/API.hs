module Network.Core.API
    ( module Network.Core.API.Internal
    , module Network.Core.API.MediaType
    , module Network.Core.API.TypeLevel
    , module Network.Core.API.Verbs
    , module Network.Core.API
    , Header
    , Status (..)
    ) where
import Data.Kind
import Data.Singletons.TypeRepTYPE ()
import Network.Core.API.Internal
import Network.Core.API.MediaType
import Network.Core.API.TypeLevel
import Network.Core.API.Verbs
import Network.HTTP.Types (Header, Status (..))

type JsonBody (a :: Type) = ReqBody JSON a

-- | Response Type synonyms

type GetJson a = Get '[ ResBody JSON a]

type PostJson a = Post '[ResBody JSON a]

type PutJson a = Put '[ ResBody JSON a]

type PatchJson a = Patch '[ ResBody JSON a]

type DeleteJson a = Delete '[ResBody JSON a]

type RawResponse v = Verb v '[ Raw ]

type EmptyResponse v = Verb v ('[ ] :: [ ResContent Type ])

-- | TODO: Doc-tests
--
-- >>> data User = User

-- >>> type Ex1 = JSONBody User :> GetJson User

-- >>> type Ex2 = Param "name" String :> GetJson User

-- >>> type Ex3 = "users" :> JSONBody User :> GetJson User

-- >>> type Ex4 = "users" :> Param "author" String :> GetJson User

-- >>> type Ex5 = Param "name" String :> Get '[ ResBody JSON User, 'ResHeaders '[ "content" := String] ]

-- >>> type Ex6 = "users" :> '[ Params '["some" := Int, "name" := String], ReqBody JSON User ] :> GetJson User
-- >> type Ex7 = "users" :> Params '[ "some" := Int, "name" :=String ] :> GetJson User
-- $setup
--
-- The doctests in this module are run with following preamble:
--
-- >>> :set -XPolyKinds
-- >>> :set -XTypeFamily
-- >>> import Network.Core.API.Internal
-- >>> import Network.Core.API.MediaType
-- >>> import Network.Core.API.TypeLevel
-- >>> import Network.Core.API.Verbs
