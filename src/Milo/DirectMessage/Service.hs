{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Milo.DirectMessage.Service (getDirectMessages) where

import Milo.Oauth1.Controller
import Milo.Model

import Control.Monad.Loops (unfoldrM)
import Data.Foldable (fold)

import Milo.Config.Model (Env)
import Milo.Cursored (CursorState(..), Cursor(..), unfoldWith)

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Client   as Client
import qualified Control.Monad.Except  as Ex

type DmCursor = Cursor T.Text

getDirectMessages :: Env -> Client.Manager -> IO (Either String DirectMessages)
getDirectMessages env manager = 
  let results   :: Ex.ExceptT String IO [[DirectMessage]] = unfoldrM (unfoldWith extractState (serviceCall env manager)) NewCursor
      flippedDM :: Maybe T.Text -> [DirectMessage] -> DirectMessages = flip DirectMessages
      flattened :: Ex.ExceptT String IO DirectMessages   = (flippedDM Nothing . fold) <$> results
  in Ex.runExceptT flattened

-- getMoreDirectMessages :: Env -> Client.Manager ->  Maybe Cursor -> IO (Either String DirectMessages)
-- getMoreDirectMessages env manager nextCursor = do
--   putStrLn $ "cursor1: " <> (show nextCursor)
--   dmsE <- performAction env manager $ dmRequestProvider nextCursor -- dm1 (5)
--   case dmsE of
--     Right dms@(DirectMessages messageList (Just nextCursor')) -> 
--       if (null messageList) then -- if it's empty then get more
--         do
--           putStrLn $ "cursor2: " <> (show nextCursor')
--           (fmap (combineDms dms)) <$> (getMoreDirectMessages (Just . Cursor $ nextCursor') env manager)
--       else pure . Right $ dms

--     Right dms@(DirectMessages _ Nothing) -> putStrLn "no more cursors" >> pure (Right dms)
--     Left dmError -> pure . Left $ dmError

-- TODO: Make DirectMessages into a Monoid

dmRequestProvider :: Maybe DmCursor -> RequestProvider IO DirectMessages
dmRequestProvider cursor = RequestProvider $ addQueryParams cursor <$> Client.parseRequest directMessagesUrl

directMessagesUrl :: String
directMessagesUrl = "https://api.twitter.com/1.1/direct_messages/events/list.json"

addQueryParams :: Maybe DmCursor -> Client.Request -> Client.Request
addQueryParams (Just cursor) = Client.setQueryString [cursorPositionParam cursor]
addQueryParams Nothing = id

cursorPositionParam :: DmCursor -> (C8.ByteString, Maybe C8.ByteString)
cursorPositionParam (Cursor pos) = ("cursor", Just . T.encodeUtf8 $ pos)

serviceCall :: Env -> Client.Manager -> Maybe DmCursor -> Ex.ExceptT String IO DirectMessages
serviceCall env manager maybeCursor = Ex.ExceptT $ performAction env manager $ dmRequestProvider maybeCursor
  

-- we only want to extract the first set of messages, so we stop once we have
-- at least one message
extractState :: DirectMessages -> ([DirectMessage], CursorState T.Text)
extractState (DirectMessages [] (Just c)) = ([], GoCursor (Cursor c)) -- No messages, but we have a cursor, then try to get more
extractState (DirectMessages [] Nothing)  = ([], StopCursor)          -- No messages and no cursor, then stop
extractState (DirectMessages msgs _)  = (msgs, StopCursor)            -- Messages so we can stop irrespective of cursor
