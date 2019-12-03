module Milo.HtmlEntity (removeHtmlEntities) where

import HTMLEntities.Decoder             (htmlEncodedText)

import Milo.Model (Tweet(..))

import qualified Data.Text.Lazy         as L
import qualified Data.Text.Lazy.Builder as LB

removeHtmlEntities :: Tweet -> Tweet
removeHtmlEntities tweet = 
  let tweetText = full_text tweet
      textWithEntities = L.toStrict . LB.toLazyText . htmlEncodedText $ tweetText
  in tweet{ full_text = textWithEntities }