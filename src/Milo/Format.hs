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

displayString :: TwitterWebUrl -> Either TweetRetrievalError TweetOutput -> String
displayString twu = docToString . displayFormat twu

displayJson :: Value -> String
displayJson = T.unpack . T.decodeUtf8 . LBS.toStrict . Pretty.encodePretty' (Pretty.defConfig { Pretty.confIndent = Pretty.Spaces 2 })

docToString :: ANSI.Doc -> String
docToString doc = ANSI.displayS (ANSI.renderPretty 0.4 80 doc) ""
  
headingFormat :: HeadingType -> ANSI.Doc
headingFormat (Heading heading)    = ANSI.onwhite $ ANSI.black (ANSI.text heading)
headingFormat (Mention handle)     = ANSI.onwhite $ ANSI.black (ANSI.text handle)
headingFormat (Search searchTerms) = ANSI.onwhite $ ANSI.black (ANSI.text "Search:" ANSI.<+> ANSI.text searchTerms)

formatTweetColored :: TwitterWebUrl -> Tweet -> ANSI.Doc
formatTweetColored (TwitterWebUrl webUrl) (Tweet created_at (TweetedBy name screen_name) id _ tweetText lang) =
  let cTweetText    = ANSI.yellow (ANSI.text tweetText)
      cTweetUserSep = ANSI.text "-"
      cUser         = ANSI.green (ANSI.text $ "@" <> screen_name)
      cUserDataSep  = ANSI.text "on"
      cDate         = ANSI.onwhite $ ANSI.black (ANSI.text created_at)
      cUrl          = ANSI.text $ T.unpack webUrl <> "/" <> T.unpack id --TODO: Handle case where trailing / is not given
      tweetDoc      = cTweetText ANSI.<+> cTweetUserSep ANSI.<+> cUser ANSI.<+> cUserDataSep ANSI.<+> cDate ANSI.<+> cUrl
  in tweetDoc 

displayFormat :: TwitterWebUrl -> Either TweetRetrievalError TweetOutput -> ANSI.Doc
displayFormat _ (Left (TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError error))) = 
  let title = headingFormat heading
      errorMessage = ANSI.text endpoint ANSI.<+> ANSI.text "failed due to:" ANSI.<+> ANSI.red (ANSI.text error)
  in title ANSI.<$$> errorMessage
displayFormat twitterWebUrl (Right (TweetOutput heading tweets)) = 
  let title = headingFormat heading
      tweetLine = intercalate "\n" $ (\(n, v) -> show n <> ". " <> (docToString . formatTweetColored twitterWebUrl . resolveReTweets $ v)) <$> zip [1..] tweets
  in ANSI.linebreak ANSI.<> title ANSI.<$$> ANSI.text tweetLine ANSI.<> ANSI.linebreak 
