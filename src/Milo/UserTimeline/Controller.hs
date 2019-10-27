module Milo.UserTimeline.Controller (userTimelineAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client         as Client
import qualified Data.Text                   as T
import qualified Data.ByteString.Char8       as C8
import Milo.UserTimeline.Service
import Milo.Format
import Data.List (intercalate)

endpoint :: String
endpoint = "User Timeline"

userTimelineAction :: Env -> Client.Manager -> MentionRequest -> IO (Either TweetRetrievalError TweetOutput)
userTimelineAction env manager mentionRequest = convertResults <$> getUserTimeline env manager mentionRequest
  where convertResults = bimap (TweetRetrievalError (Mention $ twitterHandler mentionRequest) (TwitterEndpoint endpoint) . TwitterError) (TweetOutput (Mention $ twitterHandler mentionRequest) )
  
twitterHandler :: MentionRequest -> String
twitterHandler (MentionRequest (RealName realName) (TwitterHandle twuser) _) = T.unpack realName <> " (@" <> T.unpack twuser <> ")"