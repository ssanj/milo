{-# LANGUAGE OverloadedStrings #-}

module Milo.Search.Parser.RepeatedTextTest (test_all) where

import Test.Tasty                      (TestTree)
import Test.Tasty                      (testGroup)
import Test.Tasty.HUnit                (assertFailure)
import Test.Tasty.HUnit                (testCase)
import Test.Tasty.HUnit                ((@?=))
import Text.Parsec                     (ParseError)
import Text.Parsec                     (parse)
import Text.Parsec                     (Stream)
import Text.Parsec                     (Parsec)
import Milo.Search.Parser.RepeatedText (Parser)
import Milo.Search.Parser.RepeatedText (searchTag)
import Milo.Search.Parser.RepeatedText (SearchTag(..))
import Data.Functor.Identity           (Identity)
import Data.Text                       (Text)
import Data.Text                       (pack)

test_all :: TestTree
test_all = testGroup "Repeated Text Tests" [withRetweet, withoutRT]

withRetweet :: TestTree
withRetweet =
  let t1 = "RT @TacticalGrace: Video of my talk at @Lambda_World (great conference btw!) about functional programming and blockchains https://t.co/N7FEFGLCmA #Haskell #Plutus #Cardano"
      parser = parse searchTag ""
      parseResult = parser t1
      result = 
        either (assertFailure . show) 
               (\(SearchTag key value) -> 
                  do
                    key   @?= "Video of my talk at @Lambda_World (great conference btw"
                    value @?= pack t1
               ) parseResult
  in testCase "with RT" result

withoutRT :: TestTree
withoutRT =
  let t1 = "TQ Tezos are HIRING!\n\nTQ Tezos is a New York-based blockchain technology company advancing the Tezos project.\n\n#haskell #ocaml #javascript #hiring\n\nhttps://t.co/wsLGWL0fb0"
      parser = parse searchTag ""
      parseResult = parser t1
      result = 
        either (assertFailure . show) 
               (\(SearchTag key value) -> 
                  do
                    key   @?= "TQ Tezos are HIRING"
                    value @?= pack t1
               ) parseResult
  in testCase "without RT" result
