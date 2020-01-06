{-# LANGUAGE OverloadedStrings #-}

module Milo.HtmlEntityTest (test_all) where

import Test.Tasty                      (TestTree)
import Test.Tasty                      (testGroup)
import Test.Tasty.HUnit                (assertFailure)
import Test.Tasty.HUnit                (testCase)
import Test.Tasty.HUnit                ((@?=))
import Data.Text                       (Text)
import Data.Text                       (pack)

import Milo.HtmlEntity                 (removeHtmlEntities)
import Milo.Model                      (Tweet(..))
import Milo.Model                      (TweetedBy(..))

test_all :: TestTree
test_all = testGroup "HtmlEntity Tests" [withGT, withAmp]

withGT :: TestTree
withGT =
  let tweetText     = "data Freer :: (*-&gt;*) -&gt; (*-&gt;*) where"
      inputTweet    = createTweet tweetText
      outputTweet   = removeHtmlEntities inputTweet
      expectedTweet = createTweet "data Freer :: (*->*) -> (*->*) where"
  in testCase "remove &gt;" $ outputTweet @?= expectedTweet

withAmp :: TestTree
withAmp =
  let tweetText     = "For me, there was a tearing point maybe 3-4 years ago where my love of coding and desire to be as effective &amp; valuable as I can possibly be started pointing in different directions. I'm still code-adjacent at least"
      inputTweet    = createTweet tweetText
      outputTweet   = removeHtmlEntities inputTweet
      expectedTweet = createTweet "For me, there was a tearing point maybe 3-4 years ago where my love of coding and desire to be as effective & valuable as I can possibly be started pointing in different directions. I'm still code-adjacent at least"
  in testCase "remove &amp;" $ outputTweet @?= expectedTweet

createTweet :: Text -> Tweet
createTweet fullText = Tweet "" (TweetedBy "" "") "" Nothing fullText "" 0 0
