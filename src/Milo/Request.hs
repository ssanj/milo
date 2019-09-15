{-# LANGUAGE OverloadedStrings #-}

module Milo.Request where

import Milo.Model
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "2")

extendedTweetParam :: (C8.ByteString, Maybe C8.ByteString)
extendedTweetParam = ("tweet_mode", Just "extended")

defaultParams :: Client.Request -> Client.Request
defaultParams = Client.setQueryString [countParam, extendedTweetParam]

defaultRequestProvider :: String -> RequestProvider IO [Tweet]
defaultRequestProvider url = RequestProvider $ defaultParams <$> Client.parseRequest url
