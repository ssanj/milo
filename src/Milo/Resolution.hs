{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Milo.Resolution (resolveReTweets) where

import qualified Data.Text as T
import Milo.Model

resolveReTweets :: Tweet -> Tweet
resolveReTweets tweet@(Tweet _ _ _ (Just retweetStatus) tweetText _) = 
  tweet { full_text = getFullText retweetStatus tweetText } where

    getFullText :: RetweetStatus -> T.Text -> T.Text
    getFullText (RetweetStatus retweetText (TweetedBy _ handle)) originalTweetText = 
      let retweetTag = "RT @" <> handle <> ":"
          (custom, _) = T.breakOn retweetTag (originalTweetText)
      in T.intercalate " " [custom, retweetTag, retweetText]
resolveReTweets tweet = tweet
