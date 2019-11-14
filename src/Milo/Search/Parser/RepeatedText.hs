{-# LANGUAGE OverloadedStrings #-}

module Milo.Search.Parser.RepeatedText (
    searchTag
  , SearchTag(..)
  , Parser
) where

import Data.Text             (Text)
import Data.Text             (pack)
import qualified Text.Parsec as P
import Text.Parsec           ((<|>))
import Control.Monad         (void)

data SearchTag = SearchTag { _key :: Text, _value :: Text } deriving Show

type Parser = P.Parsec Text ()

punctuation :: Parser Char
punctuation = P.oneOf "!.?:"

space :: Parser Char
space = P.oneOf "\t\n\r\f\v"

searchTag :: Parser SearchTag
searchTag = do
  str <- P.getInput
  -- Use 'try' here because optional fails if any input is consumed before failure
  void $ P.optional $ P.try retweetTag
  key <- pack <$> P.manyTill P.anyChar (P.try $ P.endOfLine <|> space <|> punctuation)
  pure $ SearchTag key str

retweetTag :: Parser ()
retweetTag = do
  void $ P.string "RT @"
  void $ P.manyTill P.anyChar (P.try $ P.char ':')
  void $ P.skipMany $ P.char ' '
