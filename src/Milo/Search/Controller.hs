module Milo.Search.Controller (searchAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Client as Client
import Milo.Search.Service
import Milo.Format

endpoint :: String
endpoint = "Search"

searchAction :: Env -> Client.Manager -> SearchRequest -> TweetResultIO Tweet
searchAction env manager searchRequest = convertResults <$> getSearch env manager searchRequest
  where convertResults = bimap (TweetRetrievalError (Search $ getSearchCriteria searchRequest) (TwitterEndpoint endpoint) . TwitterError) (TweetOutput (Search $ getSearchCriteria searchRequest) . statuses)
        getSearchCriteria (SearchRequest (SearchCriteria searchCriteria) _) = T.unpack searchCriteria