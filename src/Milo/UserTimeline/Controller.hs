module Milo.UserTimeline.Controller (userTimelineAction) where

import Milo.Model
import Data.Bifunctor (bimap)
import qualified Network.HTTP.Client as Client
import Milo.UserTimeline.Service
import Milo.Format
import Data.List (intercalate)

endpoint :: String
endpoint = "User Timeline"

userTimelineAction :: Env -> Client.Manager -> TwitterHandle -> IO (Either TweetRetrievalError TweetOutput)
userTimelineAction env manager twuser = convertResults <$> getUserTimeline env manager twuser
  where convertResults = bimap (TweetRetrievalError (show twuser) endpoint) (TweetOutput (show twuser))
  