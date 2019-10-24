module Network.Core.Http.RunHttp where

import Network.Core.Http.HttpError
import Network.Core.Http.Request
import Network.Core.Http.Response

class Monad m => RunHttp m where
  runRequest :: Request -> m Response

  throwHttpError :: HttpError -> m b

  checkResponse :: Request -> Response -> m (Maybe HttpError)
