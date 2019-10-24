module Network.Core.Http.BaseUrl where

import Data.List (isPrefixOf)

data Scheme =
    Http
  | Https
  deriving (Show, Eq, Ord)

-- | Simple data type to represent the target of HTTP requests
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme   -- ^ URI scheme to use
  , baseUrlHost   :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort   :: Int      -- ^ port (eg 80)
  , baseUrlPath   :: String   -- ^ path (eg "/a/b/c")
  } deriving (Show, Eq, Ord)

showBaseUrl :: BaseUrl -> String
showBaseUrl (BaseUrl urlscheme host port path) =
  schemeString ++ "//" ++ host ++ (portString </> path)
    where
      a </> b = if "/" `isPrefixOf` b || null b then a ++ b else a ++ '/':b

      schemeString = case urlscheme of
        Http  -> "http:"
        Https -> "https:"

      portString = case (urlscheme, port) of
        (Http, 80)   -> ""
        (Https, 443) -> ""
        _            -> ":" ++ show port
