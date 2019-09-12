module Milo.UserTimeline.Controller (userTimelineAction) where

import Milo.Model
import qualified Network.HTTP.Client as Client
import Milo.UserTimeline.Service
import Milo.UserTimeline.Format
import Data.List (intercalate)

userTimelineAction :: Env -> Client.Manager -> TwitterUser -> IO ()
userTimelineAction env manager tuser = do
  result <- getUserTimeline env manager tuser
  let output = case result of
                Left error -> "Couldn't get User Timeline of " <>  show tuser <> " due to " <> error
                Right userTimeline -> intercalate "\n" $ (\(n, v) -> show n <> ". " <> formatUserTimeline v) <$> zip [1..] userTimeline
      username = show tuser
      
  putStrLn $ username <> "\n" <> output
