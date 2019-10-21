{-# LANGUAGE OverloadedStrings #-}

module Milo.Search.Service (getSearch) where

import Milo.Oauth2.Controller
import Milo.Config
import Milo.Model
import qualified Milo.Request as R
import Milo.Oauth2.Model

import qualified Data.Text.Encoding          as T
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.TLS     as Client
import qualified Network.HTTP.Types          as Client

getSearch :: Env -> Client.Manager -> SearchRequest -> IO (Either String TwitterSearchResult)
getSearch env manager = performAction env manager . searchRequestProvider

--- get search 
searchUrl :: String
searchUrl = "https://api.twitter.com/1.1/search/tweets.json"

searchRequestProvider :: SearchRequest -> RequestProvider IO TwitterSearchResult
searchRequestProvider searchRequest =
  RequestProvider $ addQueryParams searchRequest <$> Client.parseRequest searchUrl

addQueryParams :: SearchRequest -> Client.Request -> Client.Request
addQueryParams (SearchRequest searchCriteria searchHitCount) = Client.setQueryString [numHits searchHitCount, searchParam searchCriteria, R.extendedTweetParam]

searchParam :: SearchCriteria -> (C8.ByteString, Maybe C8.ByteString)
searchParam (SearchCriteria searchCriteria) = ("q", Just . T.encodeUtf8 $ searchCriteria)

numHits :: SearchHitCount -> (C8.ByteString, Maybe C8.ByteString)
numHits (SearchHitCount count) = ("count", Just . C8.pack $ show count)
