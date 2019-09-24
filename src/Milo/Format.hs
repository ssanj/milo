module Milo.Format (displayString, displayJson) where

import Milo.Model
import Milo.Resolution                        (resolveReTweets)
import Data.List                              (intercalate)
import Data.Aeson                             (Value)
import qualified Data.Maybe                   (maybe)
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Char8        as C8
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import Data.Aeson.Encode.Pretty               as Pretty

displayString :: Either TweetRetrievalError TweetOutput -> String
displayString = docToString . displayFormat

displayJson :: Value -> String
displayJson = T.unpack . T.decodeUtf8 . LBS.toStrict . Pretty.encodePretty' (Pretty.defConfig { Pretty.confIndent = Pretty.Spaces 2 })

docToString :: ANSI.Doc -> String
docToString doc = ANSI.displayS (ANSI.renderPretty 0.4 80 doc) ""
  
headingFormat :: HeadingType -> ANSI.Doc
headingFormat (Heading heading)    = ANSI.onwhite $ ANSI.black (ANSI.text heading)
headingFormat (Mention handle)     = ANSI.onwhite $ ANSI.black (ANSI.text handle)
headingFormat (Search searchTerms) = ANSI.onwhite $ ANSI.black (ANSI.text "Search:" ANSI.<+> ANSI.text searchTerms)

formatTweetColored :: Tweet -> ANSI.Doc
formatTweetColored (Tweet created_at (TweetedBy name screen_name) _ tweetText lang) =
  let cTweetText    = ANSI.yellow (ANSI.text tweetText)
      cTweetUserSep = ANSI.text "-"
      cUser         = ANSI.green (ANSI.text $ "@" <> screen_name)
      cUserDataSep  = ANSI.text "on"
      cDate         = ANSI.onwhite $ ANSI.black (ANSI.text created_at)
      tweetDoc      = cTweetText ANSI.<+> cTweetUserSep ANSI.<+> cUser ANSI.<+> cUserDataSep ANSI.<+> cDate
  in tweetDoc 

displayFormat :: Either TweetRetrievalError TweetOutput -> ANSI.Doc
displayFormat (Left (TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError error))) = 
  let title = headingFormat heading
      errorMessage = ANSI.text endpoint ANSI.<+> (ANSI.text "failed due to:") ANSI.<+> ANSI.red (ANSI.text error)
  in title ANSI.<$$> errorMessage
displayFormat (Right (TweetOutput heading tweets)) = 
  let title = headingFormat heading
      tweetLine = intercalate "\n" $ (\(n, v) -> show n <> ". " <> (docToString . formatTweetColored . resolveReTweets $ v)) <$> zip [1..] tweets
  in ANSI.linebreak ANSI.<> title ANSI.<$$> (ANSI.text tweetLine) ANSI.<> ANSI.linebreak 
