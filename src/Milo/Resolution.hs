{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Milo.Resolution (resolveReTweets) where

import qualified Data.Text as T
import Milo.Model

resolveReTweets :: Tweet -> Tweet
resolveReTweets tweet@(Tweet _ _ _ (Just retweetStatus) tweetText _) = 
  tweet { full_text = getFullText retweetStatus tweetText } where

    getFullText :: RetweetStatus -> String -> String
    getFullText (RetweetStatus retweetText (TweetedBy _ handle)) tweetText = 
      let retweetTag = "RT @" <> T.pack handle <> ":"
          (custom, retweet) = T.breakOn retweetTag (T.pack tweetText)
      in T.unpack $ T.intercalate " " [custom, retweetTag, T.pack retweetText]
resolveReTweets tweet = tweet