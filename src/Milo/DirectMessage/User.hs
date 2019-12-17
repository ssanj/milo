{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Milo.DirectMessage.User (
    convertResults
  , getUniqueUsers
  , setEntityNames
) where

import Milo.Model

import Data.Bifunctor (bimap)

import qualified Milo.Model as MO (TwitterUser(id_str))
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Text           as T

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

convertResults :: T.Text -> Either String DirectMessages -> TweetResult DirectMessage
convertResults endpoint = bimap (\e -> TweetRetrievalError (heading endpoint) (TwitterEndpoint endpoint) (TwitterError $ T.pack e)) (TweetOutput (heading endpoint) . messages)

getUniqueUsers :: DirectMessages -> S.Set EntityId
getUniqueUsers dms = S.fromList $ messages dms >>= getDmUsers


heading :: T.Text -> Heading
heading = Heading DirectMessageHeading

getDmUsers :: DirectMessage -> [EntityId]
getDmUsers = getDmiUsers . message_info

getDmiUsers :: DirectMessageInfo -> [EntityId]
getDmiUsers dmi = [entityId . recipient $ dmi, entityId . sender $ dmi]