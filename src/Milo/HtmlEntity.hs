module Milo.HtmlEntity (removeHtmlEntities) where

import HTMLEntities.Decoder             (htmlEncodedText)

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as L
import qualified Data.Text.Lazy.Builder as LB

import Milo.Model (Tweet(..))

removeHtmlEntities :: Tweet -> Tweet
removeHtmlEntities tweet = 
  let tweetText = full_text tweet
      textWithEntities = L.unpack . LB.toLazyText . htmlEncodedText . T.pack $ tweetText
  in tweet{ full_text = textWithEntities }