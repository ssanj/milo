module Milo.Format where

import Milo.Model

formatTweet :: Tweet -> String
formatTweet (Tweet created_at (TweetedBy name screen_name) text lang) =
  text <> " - @" <> screen_name <> " on " <> created_at
