{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Milo.DirectMessage.Controller (directMessagesAction) where

import Milo.Model
import Milo.DirectMessage.Service

import Data.Bifunctor (first)

import Milo.Config.Model (Env)
import Milo.User.Service (getUsers)
import Milo.DirectMessage.User (convertResults, getUniqueUsers, setEntityNames)

import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Network.HTTP.Client as Client
import qualified Data.List.NonEmpty  as NE
import qualified Data.Maybe          as MB

endpoint :: T.Text
endpoint = "Direct Messages"

-- use EitherT/ExceptT here
directMessagesAction :: Env -> Client.Manager -> TweetResultIO DirectMessage
directMessagesAction env manager = do
  dmsE <- getDirectMessages env manager
  resultE <- case dmsE of
    Right dms -> 
      let uniqueUsersMaybe :: Maybe (NE.NonEmpty EntityId) = NE.nonEmpty . S.toList $ getUniqueUsers dms
          usersIOE = MB.maybe noUsers (hasUsers env manager) uniqueUsersMaybe 
      in
      do
        usersE <- usersIOE
        case usersE of
          Right tusers    -> pure $ Right $ setEntityNames dms tusers :: IO (Either T.Text DirectMessages)
          Left userErrors -> pure . Left $ userErrors  :: IO (Either T.Text DirectMessages)

    Left dmErrors   -> pure . Left $ T.pack dmErrors :: IO (Either T.Text DirectMessages)
  pure . convertResults endpoint . first T.unpack $ resultE


noUsers :: IO (Either T.Text [TwitterUser])
noUsers = pure . Right $ []

hasUsers :: Env -> Client.Manager -> NE.NonEmpty EntityId -> IO (Either T.Text [TwitterUser])
hasUsers = getUsers
