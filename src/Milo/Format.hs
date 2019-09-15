module Milo.Format where

import Milo.Model
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI

formatTweet :: Tweet -> String
formatTweet (Tweet created_at (TweetedBy name screen_name) text lang) =
  text <> " - @" <> screen_name <> " on " <> created_at

formatTweetColored :: Tweet -> String
formatTweetColored (Tweet created_at (TweetedBy name screen_name) tweetText lang) =
  let cTweetText    = ANSI.yellow (ANSI.text tweetText)
      cTweetUserSep = ANSI.text "-"
      cUser         = ANSI.green (ANSI.text $ "@" <> screen_name)
      cUserDataSep  = ANSI.text "on"
      cDate         = ANSI.onwhite $ ANSI.black (ANSI.text created_at)
      tweetDoc      = cTweetText ANSI.<+> cTweetUserSep ANSI.<+> cUser ANSI.<+> cUserDataSep ANSI.<+> cDate
  in ANSI.displayS (ANSI.renderPretty 0.4 80 tweetDoc) ""
  
