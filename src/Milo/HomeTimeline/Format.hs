module Milo.HomeTimeline.Format where

import Milo.HomeTimeline.Model

formatHomeTimeline :: HomeTimeline -> String
formatHomeTimeline (HomeTimeline created_at (HomeTimeLineUser name screen_name) text lang) =
  text <> " - @" <> screen_name <> " on " <> created_at
