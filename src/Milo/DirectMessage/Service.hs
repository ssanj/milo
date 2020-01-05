{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Milo.DirectMessage.Service (getDirectMessages) where

import Milo.Oauth1.Controller
import Milo.Model

import Control.Monad.Loops (unfoldrM)
import Control.Monad (when)
import Data.Foldable (fold)

import Milo.Config.Model (Env, debugSet)
import Milo.Cursored (CursorState(..), Cursor(..), unfoldWith)

import qualified Data.List             as L
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Client   as Client
import qualified Control.Monad.Except  as Ex
import qualified Control.Monad.Writer  as Wr


type DmCursor = Cursor T.Text

type DmIO = Ex.ExceptT String (Wr.WriterT [String] IO)

-- TODO: Move log handling out of here into somewhere central

getDirectMessages :: Env -> Client.Manager -> IO (Either String DirectMessages)
getDirectMessages env manager = 
  let results   :: DmIO [[DirectMessage]] = unfoldrM (unfoldWith extractState (serviceCall env manager)) NewCursor
      flippedDM :: Maybe T.Text -> [DirectMessage] -> DirectMessages = flip DirectMessages
      flattened :: DmIO DirectMessages = (flippedDM Nothing . fold) <$> results

      dumpLogs :: [String] -> IO ()
      dumpLogs logs = putStrLn "DM logs:" >> putStrLn (L.intercalate "\n" logs)
  in do 
       (dmE, logs) <- Wr.runWriterT $ Ex.runExceptT flattened
       when (debugSet env) (dumpLogs logs)
       pure dmE

dmRequestProvider :: Maybe DmCursor -> RequestProvider IO DirectMessages
dmRequestProvider cursor = RequestProvider $ addQueryParams cursor <$> Client.parseRequest directMessagesUrl

directMessagesUrl :: String
directMessagesUrl = "https://api.twitter.com/1.1/direct_messages/events/list.json"

addQueryParams :: Maybe DmCursor -> Client.Request -> Client.Request
addQueryParams (Just cursor) = Client.setQueryString [cursorPositionParam cursor]
addQueryParams Nothing = id

cursorPositionParam :: DmCursor -> (C8.ByteString, Maybe C8.ByteString)
cursorPositionParam (Cursor pos) = ("cursor", Just . T.encodeUtf8 $ pos)

serviceCall :: Env -> Client.Manager -> Maybe DmCursor -> DmIO DirectMessages
serviceCall env manager maybeCursor = 
  let actionResult :: IO (Either String DirectMessages) = performAction env manager $ dmRequestProvider maybeCursor
      actionResultWithLog :: IO (Either String DirectMessages, [String]) = fmap (\e -> (e,["Got cursor: " <> (show maybeCursor)])) actionResult
  in Ex.ExceptT $ Wr.WriterT actionResultWithLog

-- we only want to extract the first set of messages, so we stop once we have
-- at least one message
extractState :: DirectMessages -> ([DirectMessage], CursorState T.Text)
extractState (DirectMessages [] (Just c)) = ([], GoCursor (Cursor c)) -- No messages, but we have a cursor, then try to get more
extractState (DirectMessages [] Nothing)  = ([], StopCursor)          -- No messages and no cursor, then stop
extractState (DirectMessages msgs _)  = (msgs, StopCursor)            -- Messages so we can stop irrespective of cursor
