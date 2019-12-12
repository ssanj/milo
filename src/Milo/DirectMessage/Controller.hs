{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Milo.DirectMessage.Controller (directMessagesAction) where

import Milo.Model
import Milo.DirectMessage.Service

import Data.Bifunctor (bimap, first)

import Milo.Config.Model (Env)
import Milo.User.Service (getUsers)

import qualified Milo.Model as MO (TwitterUser(id_str))
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Network.HTTP.Client as Client

endpoint :: T.Text
endpoint = "Direct Messages"

-- use EitherT/ExceptT here
directMessagesAction :: Env -> Client.Manager -> TweetResultIO DirectMessage
directMessagesAction env manager = do
  dmsE <- getDirectMessages env manager
  resultE <- case dmsE of
    Right dms -> 
      let usersIOE :: IO (Either T.Text [TwitterUser]) = getUsers env manager . S.toList . getUniqueUsers $ dms in
      do
        usersE <- usersIOE
        case usersE of
          Right tusers       -> pure $ Right $ setEntityNames dms tusers :: IO (Either T.Text DirectMessages)
          Left userErrors   -> pure . Left $ userErrors  :: IO (Either T.Text DirectMessages)

    Left dmErrors   -> pure . Left $ T.pack dmErrors :: IO (Either T.Text DirectMessages)
  pure . convertResults . first T.unpack $ resultE

setEntityNames :: DirectMessages -> [TwitterUser] -> DirectMessages
setEntityNames (DirectMessages dms) tusers = 
  let userMap :: M.Map EntityId TwitterUser = M.fromList $ (\tw -> (EntityId (MO.id_str tw), tw)) <$> tusers 
      updatedDms :: [DirectMessage] = ((flip replaceEntityIdWithUserName) userMap) <$> dms
  in DirectMessages updatedDms

replaceEntityIdWithUserName :: DirectMessage -> M.Map EntityId TwitterUser -> DirectMessage
replaceEntityIdWithUserName dm userMap =
  let dmMessageInfo = message_info dm
      dmRecipientId :: EntityId = entityId . recipient $ dmMessageInfo
      dmSenderId :: EntityId    = entityId . sender $ dmMessageInfo
      entityPair    = (M.lookup dmRecipientId userMap, M.lookup dmSenderId userMap)
  in 

  case entityPair of
    (Just recipientUser, Just sendUser) -> 
      let dmWithRecipientUpdate = setEntityUser Recipient recipientUser dm
      in setEntityUser Sender sendUser dmWithRecipientUpdate
    (Just recipientUser, Nothing)       -> setEntityUser Recipient recipientUser dm
    (Nothing, Just sendUser)            -> setEntityUser Sender sendUser dm
    (Nothing, Nothing)                  -> dm

-- Would lenses make this easier?
setEntityUser :: DmEntityType -> TwitterUser -> DirectMessage -> DirectMessage
setEntityUser etype tuser dm = 
  let dmMessageInfo = message_info dm
      dmRecipient   = recipient dmMessageInfo
      dmSender      = sender dmMessageInfo
  in 
  case etype of
    Recipient -> 
      dm { 
        message_info = dmMessageInfo { 
          recipient = dmRecipient { 
            entityUser = Just tuser 
          } 
        }
      }

    Sender ->
      dm {
        message_info = dmMessageInfo { 
          sender = dmSender { 
            entityUser = Just tuser 
          }
        }
      }

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