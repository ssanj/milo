module Milo.HomeTimeline.Controller (homeTimelineAction) where

import Milo.Model
import qualified Network.HTTP.Client as Client
import Milo.HomeTimeline.Service
import Milo.Format
import Data.List (intercalate)

homeTimelineAction :: Env -> Client.Manager -> IO ()
homeTimelineAction env manager = do
  result <- getHomeTimeline env manager
  let output = case result of
                Left error -> "Couldn't get Home Timeline due to " <> error
                Right tweets -> intercalate "\n" $ (\(n, v) -> show n <> ". " <> formatTweet v) <$> zip [1..] tweets
  putStrLn output
