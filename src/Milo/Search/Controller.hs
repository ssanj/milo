module Milo.Search.Controller (searchAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Client as Client
import Milo.Search.Service
import Milo.Format

endpoint :: String
endpoint = "Search"

searchAction :: Env -> Client.Manager -> SearchRequest -> IO (Either TweetRetrievalError TweetOutput)
searchAction env manager searchRequest = convertResults <$> getSearch env manager searchRequest
  where convertResults = bimap (\e -> TweetRetrievalError (Search $ getSearchCriteria searchRequest) (TwitterEndpoint endpoint) (TwitterError e)) (\search -> TweetOutput (Search $ getSearchCriteria searchRequest) (statuses search))
        getSearchCriteria (SearchRequest (SearchCriteria searchCriteria) _) = C8.unpack searchCriteria