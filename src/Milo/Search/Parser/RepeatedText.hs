{-# LANGUAGE OverloadedStrings #-}

module Milo.Search.Parser.RepeatedText (
    retweetTag
  , RetweetTag(..)
  , Parser
) where

import Data.Text                     (Text)
import Data.Text                     (pack)
import qualified Text.Parsec      as P
import Text.Parsec      ((<|>))

data RetweetTag = RetweetTag { _key :: Text, _input :: Text } 

type Parser = P.Parsec String ()

punctuation :: Parser Char
punctuation = P.oneOf "!.?:"

space :: Parser Char
space = P.oneOf "\t\n\r\f\v"

retweetTag :: Parser RetweetTag
retweetTag = do
  str <- P.getInput
  _   <- P.string "RT @"
  _   <- P.manyTill P.anyChar (P.try $ P.char ':')
  _   <- P.skipMany $ P.char ' '
  key <- P.manyTill P.anyChar (P.try $ P.endOfLine <|> space <|> punctuation)
  pure $ RetweetTag (pack key) (pack str)