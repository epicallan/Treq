-- | This module provides a safe way to construct API endpoint base URLs
module Hreq.Core.Client.BaseUrl where

import Prelude ()
import Prelude.Compat

import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T

data Scheme =
    Http
  | Https
  deriving (Show, Eq, Ord)

-- | Simple data type to represent the target of HTTP requests
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost   :: Text   -- ^ host (eg "haskell.org")
  , baseUrlPort   :: Int    -- ^ port (eg 80)
  , baseUrlPath   :: Text   -- ^ path (eg "/a/b/c")
  } deriving (Show, Eq, Ord)

showBaseUrl :: BaseUrl -> Text
showBaseUrl (BaseUrl urlscheme host port path) =
  schemeString <> "//" <> host <> (portString </> path)
    where
      (</>) :: Text -> Text -> Text
      a </> b = if "/" `T.isPrefixOf` b || T.null b then a <> b else a <> ("/" <> b)

      schemeString :: Text
      schemeString = case urlscheme of
        Http  -> "http:"
        Https -> "https:"

      portString :: Text
      portString = case (urlscheme, port) of
        (Http, 80)   -> ""
        (Https, 443) -> ""
        _            -> cs $ ":" <> show port
