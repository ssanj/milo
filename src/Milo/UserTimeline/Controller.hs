module Milo.UserTimeline.Controller (userTimelineAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client         as Client
import qualified Data.Text                   as T
import qualified Data.ByteString.Char8       as C8
import Milo.UserTimeline.Service
import Data.List (intercalate)

endpoint :: String
endpoint = "User Timeline"

userTimelineAction :: Env -> Client.Manager -> MentionRequest -> TweetResultIO Tweet
userTimelineAction env manager mentionRequest = convertResults <$> getUserTimeline env manager mentionRequest
  where 
        heading = Heading UserTimelineHeading $ twitterHandler mentionRequest
        convertResults = bimap (TweetRetrievalError heading (TwitterEndpoint endpoint) . TwitterError) 
                               (TweetOutput heading)
  
twitterHandler :: MentionRequest -> String
twitterHandler (MentionRequest (RealName realName) (TwitterHandle twuser) _) = T.unpack realName <> " (@" <> T.unpack twuser <> ")"