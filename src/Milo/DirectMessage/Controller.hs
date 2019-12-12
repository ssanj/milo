{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Milo.DirectMessage.Controller (directMessagesAction) where

import Milo.Model
import Milo.DirectMessage.Service

import Data.Bifunctor (bimap)

import Milo.Config.Model (Env)
import Milo.User.Service (getUsers)

import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Network.HTTP.Client as Client

endpoint :: T.Text
endpoint = "Direct Messages"

directMessagesAction :: Env -> Client.Manager -> TweetResultIO DirectMessage
directMessagesAction env manager = do
  errorOrDMs <- getDirectMessages env manager
  let result =  convertResults errorOrDMs
  output <- case errorOrDMs of
       Left xError -> pure $ "got an error getting names: " <> xError 
       Right dms   -> either T.unpack show <$> (getUsers env manager . S.toList . getUniqueUsers $ dms)
  putStrLn output
  pure result

heading :: Heading
heading = Heading DirectMessageHeading endpoint

convertResults :: Either String DirectMessages -> TweetResult DirectMessage
convertResults = bimap (\e -> TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError $ T.pack e)) (TweetOutput heading . messages)

getUniqueUsers :: DirectMessages -> S.Set EntityId
getUniqueUsers dms = S.fromList $ messages dms >>= getDmUsers

getDmUsers :: DirectMessage -> [EntityId]
getDmUsers = getDmiUsers . message_info

getDmiUsers :: DirectMessageInfo -> [EntityId]
getDmiUsers dmi = [entityId . recipient $ dmi, entityId . sender $ dmi]