module Milo.Config (getConfig) where

import System.Environment (getEnv)
import qualified Milo.Model as M
import Control.Monad
import qualified Data.ByteString.Char8 as C8

getConfig :: IO M.Env
getConfig = do
  clientKey         <- M.ClientKey         <$> fromEnv "clientKey"
  clientSecret      <- M.ClientSecret      <$> fromEnv "clientSecret"
  accessToken       <- M.AccessToken       <$> fromEnv "accessToken"
  accessTokenSecret <- M.AccessTokenSecret <$> fromEnv "accessTokenSecret"
  pure $ M.Env clientKey clientSecret accessToken accessTokenSecret

fromEnv :: String -> IO C8.ByteString
fromEnv = fmap C8.pack . getEnv