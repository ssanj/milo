{-# LANGUAGE OverloadedStrings #-}

module Milo.Request (
    defaultParams
  , defaultRequestProvider
  , extendedTweetParam
  , makeRequest
) where

import Control.Monad                   (when)
import Milo.Config.Model               (MiloConfig)
import Milo.Config.Model               (_debug)
import Milo.Model                      (RequestProvider(..))
import Milo.Model                      (Tweet)
import Milo.Format.Format              (displayJson)
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Client   as Client
import qualified Data.ByteString.Lazy  as LBS
import Data.Aeson (FromJSON, eitherDecodeStrict')

countParam :: (C8.ByteString, Maybe C8.ByteString)
countParam = ("count", Just "5")

extendedTweetParam :: (C8.ByteString, Maybe C8.ByteString)
extendedTweetParam = ("tweet_mode", Just "extended")

defaultParams :: Client.Request -> Client.Request
defaultParams = Client.setQueryString [countParam, extendedTweetParam]

defaultRequestProvider :: String -> RequestProvider IO [Tweet]
defaultRequestProvider url = RequestProvider $ defaultParams <$> Client.parseRequest url

makeRequest :: FromJSON a => MiloConfig -> Client.Manager -> Client.Request -> IO (Either String a)
makeRequest config manager req = do
    -- print req
    -- putStrLn $ maybe "-" show $ listToMaybe . filter (\(n, _) -> n == hAuthorization) . Client.requestHeaders $ req
    resp <- Client.httpLbs req manager
    let responseBS = (LBS.toStrict $ Client.responseBody resp)
        showDebugInfo = _debug config
    when showDebugInfo $ putStrLn $ either (\e -> "got error: " <> (show e)) displayJson (eitherDecodeStrict' responseBS)
    return $ eitherDecodeStrict' responseBS
