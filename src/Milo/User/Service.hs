{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Milo.User.Service (getUsers) where

import Milo.Oauth2.Controller
import Milo.Model

import Milo.Config.Model (Env)

import qualified Data.Bifunctor              as BI
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString             as B
import qualified Network.HTTP.Client         as Client

-- We need some way of correlating the supplied ids with the retrieved TweetedBy
-- Possibly add `id_str` to TweetedBy
getUsers :: Env -> Client.Manager -> [EntityId] -> IO (Either T.Text [TwitterUser])
getUsers env manager userIds = 
  let result :: IO (Either String [TwitterUser]) = performAction env manager $ userRequestProvider userIds
      withText :: IO (Either T.Text [TwitterUser]) = (\et -> BI.first T.pack et) <$> result
  in withText

--- get user info 
userUrl :: String
userUrl = "https://api.twitter.com/1.1/users/lookup.json"

userRequestProvider :: [EntityId] -> RequestProvider IO [TwitterUser]
userRequestProvider userIds =
  RequestProvider $ addQueryParams userIds <$> Client.parseRequest userUrl

addQueryParams :: [EntityId] -> Client.Request -> Client.Request
addQueryParams userIds = Client.setQueryString $ [userIdParam userIds]

userIdParam :: [EntityId] -> (C8.ByteString, Maybe C8.ByteString)
userIdParam userIds = ("user_id", Just $ B.intercalate "," $ T.encodeUtf8 . unEntityId <$> userIds)
