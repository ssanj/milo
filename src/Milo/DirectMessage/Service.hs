{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Milo.DirectMessage.Service (getDirectMessages) where

import Milo.Oauth1.Controller
import Milo.Model
import Milo.Config.Model (Env)

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Client   as Client

newtype Cursor = Cursor { unCursor :: T.Text } deriving Show

getDirectMessages :: Env -> Client.Manager -> IO (Either String DirectMessages)
getDirectMessages = getMoreDirectMessages Nothing

getMoreDirectMessages :: Maybe Cursor -> Env -> Client.Manager -> IO (Either String DirectMessages)
getMoreDirectMessages nextCursor env manager = do
  putStrLn $ "cursor1: " <> (show nextCursor)
  dmsE <- performAction env manager $ dmRequestProvider nextCursor -- dm1 (5)
  case dmsE of
    Right dms@(DirectMessages messageList (Just nextCursor')) -> 
      if (null messageList) then -- if it's empty then get more
        do
          putStrLn $ "cursor2: " <> (show nextCursor')
          (fmap (combineDms dms)) <$> (getMoreDirectMessages (Just . Cursor $ nextCursor') env manager)
      else pure . Right $ dms

    Right dms@(DirectMessages _ Nothing) -> putStrLn "no more cursors" >> pure (Right dms)
    Left dmError -> pure . Left $ dmError

combineDms :: DirectMessages -> DirectMessages -> DirectMessages
combineDms (DirectMessages msg1 _) (DirectMessages msg2 cursor) = DirectMessages (msg1 ++ msg2) cursor

dmRequestProvider :: Maybe Cursor -> RequestProvider IO DirectMessages
dmRequestProvider cursor = RequestProvider $ addQueryParams cursor <$> Client.parseRequest directMessagesUrl

directMessagesUrl :: String
directMessagesUrl = "https://api.twitter.com/1.1/direct_messages/events/list.json"

addQueryParams :: Maybe Cursor -> Client.Request -> Client.Request
addQueryParams (Just cursor) = Client.setQueryString [cursorPositionParam cursor]
addQueryParams Nothing = id

cursorPositionParam :: Cursor -> (C8.ByteString, Maybe C8.ByteString)
cursorPositionParam (Cursor pos) = ("cursor", Just . T.encodeUtf8 $ pos)
