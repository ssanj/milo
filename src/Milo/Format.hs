module Milo.Format (displayString, displayJson, displayDirectMessageString) where

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

-- generisize this
-- what behaviour does l need?
-- displayString :: (Formatting a, Formatting l) => TwitterWebUrl -> Either l a -> String
displayString :: TwitterWebUrl -> Either TweetRetrievalError TweetOutput -> String
displayString twu = docToString . displayFormat twu

displayDirectMessageString :: TwitterWebUrl -> Either TweetRetrievalError DirectMessages -> String
displayDirectMessageString twu = docToString . displayDirectMessageFormat twu

displayJson :: Value -> String
displayJson = T.unpack . T.decodeUtf8 . LBS.toStrict . Pretty.encodePretty' (Pretty.defConfig { Pretty.confIndent = Pretty.Spaces 2 })

docToString :: ANSI.Doc -> String
docToString doc = ANSI.displayS (ANSI.renderPretty 0.4 80 doc) ""
  
headingFormat :: HeadingType -> ANSI.Doc
headingFormat (Heading heading)    = ANSI.underline $ ANSI.white (ANSI.text heading)
headingFormat (Mention handle)     = ANSI.underline $ ANSI.white (ANSI.text handle)
headingFormat (Search searchTerms) = ANSI.underline $ ANSI.white (ANSI.text "Search:" ANSI.<+> ANSI.text searchTerms)

formatTweetColored :: TwitterWebUrl -> Tweet -> ANSI.Doc
formatTweetColored (TwitterWebUrl webUrl) (Tweet created_at (TweetedBy name screen_name) id _ tweetText lang) =
  let cTweetText    = ANSI.yellow (ANSI.text tweetText)
      cTweetUserSep = ANSI.text "-"
      cUser         = ANSI.green (ANSI.text $ "@" <> screen_name)
      cUserDataSep  = ANSI.text "on"
      cDate         = ANSI.white (ANSI.text created_at)
      cUrl          = ANSI.text $ T.unpack webUrl <> "/" <> T.unpack id --TODO: Handle case where trailing / is not given
      tweetDoc      = cTweetText ANSI.<+> cTweetUserSep ANSI.<+> cUser ANSI.<+> cUserDataSep ANSI.<+> cDate ANSI.<+> cUrl
  in tweetDoc 

formatDMColored :: TwitterWebUrl -> DirectMessage -> ANSI.Doc
formatDMColored (TwitterWebUrl webUrl) (DirectMessage created_at id (DirectMessageInfo sourceApp recipient sender dmText) messageType) =
  let cDMText      = ANSI.yellow (ANSI.text $ T.unpack dmText)
      cDMUserSep   = ANSI.text "-"
      cRecipient   = ANSI.text $ "to: " <> T.unpack recipient
      cSender      = ANSI.text $ "from: " <> T.unpack sender
      cUserDataSep = ANSI.text "on"
      cDate        = ANSI.white (ANSI.text $ T.unpack created_at)
      cMType       = ANSI.text ("(" <>  T.unpack messageType <> ")")
      cUrl         = ANSI.text $ T.unpack webUrl <> "/" <> T.unpack id --TODO: Handle case where trailing / is not given
      dmDoc        = cDMText ANSI.<+> cDMUserSep ANSI.<+> cSender ANSI.<+> cRecipient ANSI.<+> cUserDataSep ANSI.<+> cDate ANSI.<+> cUrl ANSI.<+> cMType
  in dmDoc 

-- generisize this
-- (Formatting a, Formatting l) => TwitterWebUrl -> Either l a -> ANSI.Doc
displayDirectMessageFormat :: TwitterWebUrl -> Either TweetRetrievalError DirectMessages -> ANSI.Doc
displayDirectMessageFormat _ (Left tweetRetrievalError) = displayFormatTweetRetrievalError tweetRetrievalError
displayDirectMessageFormat twitterWebUrl (Right (DirectMessages dms)) = 
  let title = headingFormat $ Heading "Direct Messages"
      dmLine = intercalate "\n\n" $ (\(n, v) -> show n <> ". " <> (docToString . formatDMColored twitterWebUrl $ v)) <$> zip [1..] dms
  in ANSI.linebreak ANSI.<> title ANSI.<$$> ANSI.text dmLine ANSI.<> ANSI.linebreak 

displayFormat :: TwitterWebUrl -> Either TweetRetrievalError TweetOutput -> ANSI.Doc
displayFormat _ (Left tweetRetrievalError) = displayFormatTweetRetrievalError tweetRetrievalError
displayFormat twitterWebUrl (Right (TweetOutput heading tweets)) = 
  let title = headingFormat heading
      tweetLine = intercalate "\n\n" $ (\(n, v) -> show n <> ". " <> (docToString . formatTweetColored twitterWebUrl . resolveReTweets $ v)) <$> zip [1..] tweets
  in ANSI.linebreak ANSI.<> title ANSI.<$$> ANSI.text tweetLine ANSI.<> ANSI.linebreak 

displayFormatTweetRetrievalError :: TweetRetrievalError -> ANSI.Doc
displayFormatTweetRetrievalError (TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError error)) =
  let title = headingFormat heading
      errorMessage = ANSI.text endpoint ANSI.<+> ANSI.text "failed due to:" ANSI.<+> ANSI.red (ANSI.text error)
  in title ANSI.<$$> errorMessage
