{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Milo.Request (
    defaultParams
  , defaultRequestProvider
  , extendedTweetParam
  , makeRequest
) where

import Control.Monad                   (when)
import Text.Printf                     (printf)
import Data.List                       (intercalate)
import Milo.Config.Model               (MiloConfig)
import Milo.Config.Model               (_debug)
import Milo.Model                      (RequestProvider(..))
import Milo.Model                      (Tweet)
import Milo.Format.Format              (displayJson)

import qualified Data.ByteString.Char8     as C8
import qualified Network.HTTP.Types.Status as ST
import qualified Network.HTTP.Client       as Client
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.CaseInsensitive      as CI
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
    when showDebugInfo $ 
      do 
        let status = Client.responseStatus resp
        putStrLn (printf "statusCode: %d, message: %s" (ST.statusCode status) (C8.unpack $ ST.statusMessage status))
        let headers = Client.responseHeaders resp 
        let headerPairs :: [String] = fmap (\(name, value) -> printf "\t %s:%s" (C8.unpack $ CI.original $ name :: String) (C8.unpack value :: String)) headers
        putStrLn "headers:"
        putStrLn $ intercalate "\n" headerPairs
        putStrLn "body:"
        putStrLn $ either (\e -> "got error: " <> (show e)) displayJson (eitherDecodeStrict' responseBS)
    return $ eitherDecodeStrict' responseBS
