{-# Language InstanceSigs #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Milo.Format.Model where

import Milo.Model
import Milo.Format.Format                     (docToString)
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import qualified Data.Text                    as T
import Data.List                              (intercalate)

data WithTwitterWebUrl a = WithTwitterWebUrl TwitterWebUrl a

instance ANSI.Pretty Heading where
  pretty (Heading MentionHeading       heading) = genericHeading heading
  pretty (Heading UserTimelineHeading  heading) = genericHeading heading 
  pretty (Heading HomeTimelineHeading  heading) = genericHeading heading 
  pretty (Heading DirectMessageHeading heading) = genericHeading heading 
  pretty (Heading SearchHeading        heading) = genericHeading $ "Search:" <> " " <> heading
  
instance ANSI.Pretty (WithTwitterWebUrl Tweet) where
  pretty :: (WithTwitterWebUrl Tweet) -> ANSI.Doc
  pretty (WithTwitterWebUrl url tweet) = formatTweetColored url tweet

instance ANSI.Pretty (WithTwitterWebUrl DirectMessage) where
  pretty :: WithTwitterWebUrl DirectMessage -> ANSI.Doc
  pretty (WithTwitterWebUrl url dm) = formatDMColored url dm

instance ANSI.Pretty TweetRetrievalError where
  pretty = displayFormatTweetRetrievalError

displayFormat :: (ANSI.Pretty (WithTwitterWebUrl a)) => TwitterWebUrl -> TweetResult a -> ANSI.Doc
displayFormat _ (Left tweetRetrievalError) = displayFormatTweetRetrievalError tweetRetrievalError
displayFormat twitterWebUrl (Right tweetOutput) =
  displayNumberedDoc (WithTwitterWebUrl twitterWebUrl tweetOutput)

displayNumberedDoc :: (ANSI.Pretty (WithTwitterWebUrl b)) => WithTwitterWebUrl (TweetOutput [] b) -> ANSI.Doc
displayNumberedDoc withTwitterWebUrl =
  let (WithTwitterWebUrl url (TweetOutput heading results)) = withTwitterWebUrl
      title = ANSI.pretty heading
      resultLine = intercalate "\n\n" $ (\(n, v) -> show n <> ". " <> (docToString . ANSI.pretty . WithTwitterWebUrl url $ v)) <$> zip [1..] results
  in ANSI.linebreak ANSI.<> title ANSI.<$$> ANSI.text resultLine ANSI.<> ANSI.linebreak 


displayFormatTweetRetrievalError :: TweetRetrievalError -> ANSI.Doc
displayFormatTweetRetrievalError (TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError error)) =
  let title = ANSI.pretty heading
      errorMessage = ANSI.text endpoint ANSI.<+> ANSI.text "failed due to:" ANSI.<+> ANSI.red (ANSI.text error)
  in title ANSI.<$$> errorMessage

genericHeading :: String -> ANSI.Doc
genericHeading = ANSI.underline . ANSI.white . ANSI.text

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

