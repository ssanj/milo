{-# Language InstanceSigs #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Milo.Format.Model (
    WithTwitterWebUrl(..)
  , displayFormat
)where

import Milo.Model

import Data.List                              (intercalate)

import Milo.Resolution                        (resolveReTweets)
import Milo.HtmlEntity                        (removeHtmlEntities)
import Milo.Format.Format                     (docToString)

import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import qualified Data.Text                    as T


data WithTwitterWebUrl a = WithTwitterWebUrl TwitterWebUrl a

displayFormatHeading :: Heading -> ANSI.Doc
displayFormatHeading (Heading MentionHeading       heading) = genericHeading heading
displayFormatHeading (Heading UserTimelineHeading  heading) = genericHeading heading 
displayFormatHeading (Heading HomeTimelineHeading  heading) = genericHeading heading 
displayFormatHeading (Heading DirectMessageHeading heading) = genericHeading heading 
displayFormatHeading (Heading SearchHeading        heading) = genericHeading $ "Search:" <> " " <> heading
  
instance ANSI.Pretty (WithTwitterWebUrl Tweet) where
  pretty :: (WithTwitterWebUrl Tweet) -> ANSI.Doc
  pretty (WithTwitterWebUrl url tweet) = formatTweetColored url (removeHtmlEntities . resolveReTweets $ tweet)

instance ANSI.Pretty (WithTwitterWebUrl DirectMessage) where
  pretty :: WithTwitterWebUrl DirectMessage -> ANSI.Doc
  pretty (WithTwitterWebUrl url dm) = formatDMColored url dm

displayFormat :: (ANSI.Pretty (WithTwitterWebUrl a)) => TwitterWebUrl -> TweetResult a -> ANSI.Doc
displayFormat _ (Left tweetRetrievalError) = displayFormatTweetRetrievalError tweetRetrievalError
displayFormat twitterWebUrl (Right tweetOutput) =
  displayNumberedDoc (WithTwitterWebUrl twitterWebUrl tweetOutput)

displayNumberedDoc :: (ANSI.Pretty (WithTwitterWebUrl b)) => WithTwitterWebUrl (TweetOutput [] b) -> ANSI.Doc
displayNumberedDoc withTwitterWebUrl =
  let (WithTwitterWebUrl url (TweetOutput heading results)) = withTwitterWebUrl
      title = displayFormatHeading heading
      resultLine = intercalate "\n\n" $ (\(n, v) -> show n <> ". " <> (docToString . ANSI.pretty . WithTwitterWebUrl url $ v)) <$> zip [1..] results
  in ANSI.linebreak ANSI.<> title ANSI.<$$> ANSI.text resultLine ANSI.<> ANSI.linebreak 


displayFormatTweetRetrievalError :: TweetRetrievalError -> ANSI.Doc
displayFormatTweetRetrievalError (TweetRetrievalError heading (TwitterEndpoint endpoint) (TwitterError apiError)) =
  let title = displayFormatHeading heading
      errorMessage = ANSI.text endpoint ANSI.<+> ANSI.text "failed due to:" ANSI.<+> ANSI.red (ANSI.text apiError)
  in title ANSI.<$$> errorMessage

genericHeading :: String -> ANSI.Doc
genericHeading = ANSI.underline . ANSI.white . ANSI.text

formatTweetColored :: TwitterWebUrl -> Tweet -> ANSI.Doc
formatTweetColored (TwitterWebUrl webUrl) (Tweet createdAt (TweetedBy _ screenName) idStr _ tweetText _) =
  let cTweetText    = ANSI.yellow (ANSI.text tweetText)
      cTweetUserSep = ANSI.text "-"
      cUser         = ANSI.green (ANSI.text $ "@" <> screenName)
      cUserDataSep  = ANSI.text "on"
      cDate         = ANSI.white (ANSI.text createdAt)
      cUrl          = ANSI.text $ T.unpack webUrl <> "/" <> T.unpack idStr --TODO: Handle case where trailing / is not given
      tweetDoc      = cTweetText ANSI.<+> cTweetUserSep ANSI.<+> cUser ANSI.<+> cUserDataSep ANSI.<+> cDate ANSI.<+> cUrl
  in tweetDoc 

formatDMColored :: TwitterWebUrl -> DirectMessage -> ANSI.Doc
formatDMColored (TwitterWebUrl webUrl) (DirectMessage createdAt idStr (DirectMessageInfo _ recipient sender dmText) messageType) =
  let cDMText      = ANSI.yellow (ANSI.text $ T.unpack dmText)
      cDMUserSep   = ANSI.text "-"
      cRecipient   = ANSI.text $ "to: " <> T.unpack recipient
      cSender      = ANSI.text $ "from: " <> T.unpack sender
      cUserDataSep = ANSI.text "on"
      cDate        = ANSI.white (ANSI.text $ T.unpack createdAt)
      cMType       = ANSI.text ("(" <>  T.unpack messageType <> ")")
      cUrl         = ANSI.text $ T.unpack webUrl <> "/" <> T.unpack idStr --TODO: Handle case where trailing / is not given
      dmDoc        = cDMText ANSI.<+> cDMUserSep ANSI.<+> cSender ANSI.<+> cRecipient ANSI.<+> cUserDataSep ANSI.<+> cDate ANSI.<+> cUrl ANSI.<+> cMType
  in dmDoc 

