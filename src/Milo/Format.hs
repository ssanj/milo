module Milo.Format where

import Milo.Model
import Data.List (intercalate)
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI

formatTweet :: Tweet -> String
formatTweet (Tweet created_at (TweetedBy name screen_name) text lang) =
  text <> " - @" <> screen_name <> " on " <> created_at

formatTweetColored :: Tweet -> ANSI.Doc
formatTweetColored (Tweet created_at (TweetedBy name screen_name) tweetText lang) =
  let cTweetText    = ANSI.yellow (ANSI.text tweetText)
      cTweetUserSep = ANSI.text "-"
      cUser         = ANSI.green (ANSI.text $ "@" <> screen_name)
      cUserDataSep  = ANSI.text "on"
      cDate         = ANSI.onwhite $ ANSI.black (ANSI.text created_at)
      tweetDoc      = cTweetText ANSI.<+> cTweetUserSep ANSI.<+> cUser ANSI.<+> cUserDataSep ANSI.<+> cDate
  in tweetDoc

displayString :: Either TweetRetrievalError TweetOutput -> String
displayString = docToString . displayFormat

docToString :: ANSI.Doc -> String
docToString doc = ANSI.displayS (ANSI.renderPretty 0.4 80 doc) ""
  
headingFormat :: HeadingType -> ANSI.Doc
headingFormat (Heading heading)    = ANSI.onwhite $ ANSI.black (ANSI.text heading)
headingFormat (Mention handle)     = ANSI.onwhite $ ANSI.black (ANSI.text handle)
headingFormat (Search searchTerms) = ANSI.onwhite $ ANSI.black (ANSI.text "Search:" ANSI.<+> ANSI.text searchTerms)

displayFormat :: Either TweetRetrievalError TweetOutput -> ANSI.Doc
displayFormat (Left (TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError error))) = 
  let title = headingFormat heading
      errorMessage = ANSI.text endpoint ANSI.<+> (ANSI.text "failed due to:") ANSI.<+> ANSI.red (ANSI.text error)
  in title ANSI.<$$> errorMessage

displayFormat (Right (TweetOutput heading tweets)) = 
  let title = headingFormat heading
      tweetLine = intercalate "\n" $ (\(n, v) -> show n <> ". " <> (docToString $ formatTweetColored v)) <$> zip [1..] tweets
  in ANSI.linebreak ANSI.<> title ANSI.<$$> (ANSI.text tweetLine) ANSI.<> ANSI.linebreak 
