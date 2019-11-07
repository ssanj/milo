module Milo.Search.Controller (searchAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import qualified Network.HTTP.Client as Client
import Milo.Search.Service
import Milo.Config.Model (Env)

endpoint :: String
endpoint = "Search"

searchAction :: Env -> Client.Manager -> SearchRequest -> TweetResultIO Tweet
searchAction env manager searchRequest = convertResults <$> getSearch env manager searchRequest
  where 
        heading = Heading SearchHeading $ getSearchCriteria searchRequest

        convertResults :: Either String TwitterSearchResult -> TweetResult Tweet
        convertResults = bimap (TweetRetrievalError heading (TwitterEndpoint endpoint) . TwitterError) 
                               (TweetOutput heading . statuses)
        getSearchCriteria (SearchRequest (SearchCriteria searchCriteria) _) = T.unpack searchCriteria