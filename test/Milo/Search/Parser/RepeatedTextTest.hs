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

newtype OriginalText      = OriginalText Text
newtype ExpectedSearchKey = ExpectedSearchKey Text
newtype TestCaseName      = TestCaseName String
type NamedTest            = TestCaseName -> TestTree

test_all :: TestTree
test_all = 
  testGroup 
    "Repeated Text Tests" 
    [
        withRetweet      $ TestCaseName "withRT"
      , withoutRT        $ TestCaseName "without RT"
      , withEmoji        $ TestCaseName "withEmoji" 
      , withThreeElipsis $ TestCaseName "withThreeElipsis"]

withRetweet :: NamedTest
withRetweet = 
  testRepeats 
    (OriginalText "RT @TacticalGrace: Video of my talk at @Lambda_World (great conference btw!) about functional programming and blockchains https://t.co/N7FEFGLCmA #Haskell #Plutus #Cardano")
    (ExpectedSearchKey "Video of my talk at @Lambda_World (great conference btw")

withoutRT :: NamedTest
withoutRT =
  testRepeats 
    (OriginalText "TQ Tezos are HIRING!\n\nTQ Tezos is a New York-based blockchain technology company advancing the Tezos project.\n\n#haskell #ocaml #javascript #hiring\n\nhttps://t.co/wsLGWL0fb0")
    (ExpectedSearchKey "TQ Tezos are HIRING")

withEmoji :: NamedTest
withEmoji =
  testRepeats
    (OriginalText "I have been working on a type driven #haskell HTTP client library inspired by servant-client for some time now and today is the initial public release day \x1F642\x1F973.\n\nIntroductory blog" )
    (ExpectedSearchKey "I have been working on a type driven #haskell HTTP client library inspired by servant-client for some time now and today is the initial public release day \x1F642\x1F973")

withThreeElipsis :: NamedTest
withThreeElipsis =
  testRepeats
    (OriginalText "RT @arnabch01: @angelicagallegs @YukariKingdom18 @mhall55nine @eoff_sylvia @MontyNishimura @BPerrionni @semicvet50 @BrindusaB1 @RitaCobix @…")
    (ExpectedSearchKey "@angelicagallegs @YukariKingdom18 @mhall55nine @eoff_sylvia @MontyNishimura @BPerrionni @semicvet50 @BrindusaB1 @RitaCobix @")

testRepeats :: OriginalText -> ExpectedSearchKey -> TestCaseName -> TestTree
testRepeats (OriginalText originalText) (ExpectedSearchKey expectedSearchKey) (TestCaseName testCaseName) =
  let parseResult = parse searchTag "" originalText
      result = 
        either (assertFailure . show)
               (\(SearchTag key value) ->
                 do
                   key   @?= expectedSearchKey
                   value @?= originalText
               ) parseResult
  in testCase testCaseName result


