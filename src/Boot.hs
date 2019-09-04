module Boot where

import System.Environment (getEnv)
import qualified Model as M
import Control.Monad
import qualified Data.ByteString.Char8 as C8

getConfig :: IO M.Env
getConfig = do
  clientKey         <- M.ClientKey . C8.pack <$> getEnv "clientKey"
  clientSecret      <- M.ClientSecret . C8.pack <$> getEnv "clientSecret"
  accessToken       <- M.AccessToken . C8.pack <$> getEnv "accessToken"
  accessTokenSecret <- M.AccessTokenSecret . C8.pack <$> getEnv "accessTokenSecret"
  pure $ M.Env clientKey clientSecret accessToken accessTokenSecret
