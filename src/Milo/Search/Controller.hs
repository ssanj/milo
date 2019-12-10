{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Milo.Search.Controller (searchAction) where

import Prelude                                   hiding (log)
import Control.Monad                             (when)
import Data.Either                               (rights)
import Data.Bifunctor                            (bimap)
import Text.Parsec                               (parse)
import Text.Parsec                               (ParseError)

import Milo.Model                                (Tweet(full_text), TwitterSearchResult(statuses), twitterError) 
import Milo.Search.Service                       (getSearch)
import Milo.Config.Model                         (Env, debugSet)

import qualified Data.Map.Strict                 as MS
import qualified Data.Text                       as T
import qualified Network.HTTP.Client             as Client

import qualified Milo.Search.Parser.RepeatedText as R
import qualified Milo.Model                      as M 

endpoint :: T.Text
endpoint = "Search"

data DebugInfo = DebugInfo {
    _searchTagTweetEPairs :: [Either ParseError (M.Tweet, R.SearchTag)]
  , _searchTagTweetPairs :: [(M.Tweet, R.SearchTag)]
  , _keyTagPairs :: [(T.Text, M.Tweet)]
  , _uniqueTweetMap :: MS.Map T.Text M.Tweet
}

data FilteredTweets = FilteredTweets DebugInfo [M.Tweet]

searchAction :: Env -> Client.Manager -> M.SearchRequest -> M.TweetResultIO M.Tweet
searchAction env manager searchRequest = do 
  searchResultE <- getSearch env manager searchRequest
  case convertResults searchRequest searchResultE of
    Left e -> pure . Left $ e
    (Right (debugInfo, tweetResult)) -> do
      logDebugInfo (debugSet env) debugInfo
      pure . Right $ tweetResult

logDebugInfo :: Bool -> DebugInfo -> IO ()
logDebugInfo debug (DebugInfo searchTagTweetEPairs searchTagTweetPairs keyTagPairs uniqueTweetMap) = 
  let log = "\nsearchTagTweetEPairs: " <> show searchTagTweetEPairs <> 
            "\nsearchTagTweetPairs: " <> show searchTagTweetPairs <> 
            "\nkeyPairs: " <>  show keyTagPairs <> 
            "\nunqiueTweetMap: " <> show uniqueTweetMap
  in when debug (putStrLn log)

heading :: M.SearchRequest -> M.Heading
heading = M.Heading M.SearchHeading . getSearchCriteria

convertResults :: M.SearchRequest -> Either String M.TwitterSearchResult -> Either M.TweetRetrievalError (DebugInfo, M.TweetOutput [] M.Tweet)
convertResults searchRequest = 
  let mkHeading = heading searchRequest in
  bimap (M.TweetRetrievalError mkHeading (M.TwitterEndpoint endpoint) . twitterError) 
        (\tsr -> 
           let tweets = statuses tsr
               (FilteredTweets debugInfo filteredTweets) = removeDuplicates tweets
               -- limit hits to the number requested
               hitCountRequested = getSearchHitCount searchRequest
           in (debugInfo, M.TweetOutput mkHeading $ take hitCountRequested filteredTweets)
        )
        
getSearchCriteria :: M.SearchRequest -> T.Text
getSearchCriteria (M.SearchRequest (M.SearchCriteria searchCriteria) _) = searchCriteria

getSearchHitCount :: M.SearchRequest -> Int
getSearchHitCount (M.SearchRequest _ (M.SearchHitCount hitCount)) = hitCount


removeDuplicates :: [M.Tweet] -> FilteredTweets
removeDuplicates tweets =
  let searchTagTweetEPairs :: [Either ParseError (M.Tweet, R.SearchTag)] = (\tweet -> (tweet,) <$> (parse R.searchTag "" . full_text $ tweet)) <$> tweets
      
      searchTagTweetPairs  :: [(M.Tweet, R.SearchTag)] = rights searchTagTweetEPairs
      
      keyTagPairs :: [(T.Text, M.Tweet)] = (\(tweet, (R.SearchTag key _)) -> (key, tweet)) <$> searchTagTweetPairs
      
      uniqueTweetMap :: MS.Map T.Text M.Tweet = MS.fromList keyTagPairs
      
      debugInfo = DebugInfo searchTagTweetEPairs searchTagTweetPairs keyTagPairs uniqueTweetMap
  in FilteredTweets debugInfo $ MS.elems uniqueTweetMap