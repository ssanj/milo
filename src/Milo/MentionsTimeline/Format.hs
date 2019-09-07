module Milo.MentionsTimeline.Format where

import Milo.MentionsTimeline.Model

formatMentionsTimeline :: MentionsTimeline -> String
formatMentionsTimeline (MentionsTimeline created_at (MentionsTimelineUser name screen_name) text lang) =
  text <> " - @" <> screen_name <> " on " <> created_at
