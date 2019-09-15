module Milo.MentionsTimeline.Controller (mentionsTimelineAction) where

import Milo.Model
import qualified Network.HTTP.Client as Client
import Milo.MentionsTimeline.Service
import Milo.Format
import Data.List (intercalate)

mentionsTimelineAction :: Env -> Client.Manager -> IO ()
mentionsTimelineAction env manager = do
  result <- getMentionsTimeline env manager
  let output = case result of
                Left error -> "Couldn't get Mentions Timeline due to " <> error
                Right tweets -> intercalate "\n" $ (\(n, v) -> show n <> ". " <> formatTweet v) <$> zip [1..] tweets
  putStrLn output
