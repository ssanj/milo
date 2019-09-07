module Milo.MentionsTimeline.Controller (mentionsTimelineAction) where

import Milo.Model
import qualified Network.HTTP.Client as Client
import Milo.MentionsTimeline.Service
import Milo.MentionsTimeline.Format
import Data.List (intercalate)

mentionsTimelineAction :: Env -> Client.Manager -> IO ()
mentionsTimelineAction env manager = do
  result <- getMentionsTimeline env manager
  let output = case result of
                Left error -> "Couldn't get Mentions Timeline due to " <> error
                Right mentionsTimeline -> intercalate "\n" $ (\(n, v) -> show n <> ". " <> formatMentionsTimeline v) <$> zip [1..] mentionsTimeline
  putStrLn output
