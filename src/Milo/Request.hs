{-# LANGUAGE OverloadedStrings #-}

module Milo.Request where

import Milo.Model
import Milo.Format                           (displayJson)
import qualified Data.ByteString.Char8       as C8
import qualified Network.HTTP.Client         as Client
import qualified Data.ByteString.Lazy        as LBS
import Data.Aeson (FromJSON, eitherDecodeStrict', Value)

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "2")

extendedTweetParam :: (C8.ByteString, Maybe C8.ByteString)
extendedTweetParam = ("tweet_mode", Just "extended")

defaultParams :: Client.Request -> Client.Request
defaultParams = Client.setQueryString [countParam, extendedTweetParam]

defaultRequestProvider :: String -> RequestProvider IO [Tweet]
defaultRequestProvider url = RequestProvider $ defaultParams <$> Client.parseRequest url

makeRequest :: FromJSON a => Client.Manager -> Client.Request -> IO (Either String a)
makeRequest manager req = do
    -- print req
    -- putStrLn $ maybe "-" show $ listToMaybe . filter (\(n, _) -> n == hAuthorization) . Client.requestHeaders $ req
    resp <- Client.httpLbs req manager
    let responseE = eitherDecodeStrict' (LBS.toStrict $ Client.responseBody resp)
    putStrLn $ either (\e -> "got error: " <> (show e)) displayJson responseE
    return $ eitherDecodeStrict' (LBS.toStrict $ Client.responseBody resp)
