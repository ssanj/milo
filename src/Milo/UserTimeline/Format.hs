module Milo.UserTimeline.Format where

import Milo.UserTimeline.Model

formatUserTimeline :: UserTimeline -> String
formatUserTimeline (UserTimeline created_at (UserTimelineUser name screen_name) text lang) =
  text <> " - @" <> screen_name <> " on " <> created_at
