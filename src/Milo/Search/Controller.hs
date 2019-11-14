{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Milo.Search.Controller (searchAction) where

import Data.Either                               (rights)
import Data.Bifunctor                            (bimap)
import Text.Parsec                               (parse)
import Text.Parsec                               (ParseError)
import qualified Debug.Trace                     as D
import qualified Data.Map.Strict                 as MS
import qualified Data.Text                       as T
import qualified Network.HTTP.Client             as Client

import Milo.Model                                (Tweet(full_text)) 
import Milo.Model                                (TwitterSearchResult(statuses)) 
import Milo.Search.Service                       (getSearch)
import Milo.Config.Model                         (Env)
import qualified Milo.Search.Parser.RepeatedText as R
import qualified Milo.Model                      as M 

endpoint :: String
endpoint = "Search"

searchAction :: Env -> Client.Manager -> M.SearchRequest -> M.TweetResultIO M.Tweet
searchAction env manager searchRequest = convertResults <$> getSearch env manager searchRequest
  where 
        heading = M.Heading M.SearchHeading $ getSearchCriteria searchRequest

        convertResults :: Either String M.TwitterSearchResult -> M.TweetResult M.Tweet
        convertResults = bimap (M.TweetRetrievalError heading (M.TwitterEndpoint endpoint) . M.TwitterError) 
                               (M.TweetOutput heading . filterRepeats . statuses)
        getSearchCriteria (M.SearchRequest (M.SearchCriteria searchCriteria) _) = T.unpack searchCriteria

        filterRepeats :: [M.Tweet] -> [M.Tweet]
        filterRepeats tweets =
          let searchTagTweetEPairs :: [Either ParseError (M.Tweet, R.SearchTag)] = (\tweet -> (tweet,) <$> (parse R.searchTag "" . T.pack . full_text $ tweet)) <$> tweets
              searchTagTweetPairs  :: [(M.Tweet, R.SearchTag)] = rights searchTagTweetEPairs
              keyTagPairs :: [(T.Text, M.Tweet)] = (\(tweet, (R.SearchTag key _)) -> (key, tweet)) <$> searchTagTweetPairs
              uniqueTweetMap :: MS.Map T.Text M.Tweet = MS.fromList keyTagPairs
              traceOutput = "searchTagTweetEPairs: " <> (show searchTagTweetEPairs) <> 
                            "\nkeyTagPairs: " <> (show keyTagPairs) <> 
                            "\nuniqueTweetMap: " <> (show uniqueTweetMap)
          in D.trace traceOutput $ MS.elems uniqueTweetMap