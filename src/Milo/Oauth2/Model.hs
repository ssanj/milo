{-# LANGUAGE DeriveGeneric #-}

module Milo.Oauth2.Model where

import qualified Data.ByteString.Char8 as C8
import Milo.Model
import GHC.Generics
import Data.Aeson (FromJSON)

data OAuth2ClientToken = 
  OAuth2ClientToken { key :: ClientKey, secret :: ClientSecret } deriving (Show)

data BearerToken = BearerToken { token_type :: !String, access_token :: !String } deriving (Show, Generic)

instance FromJSON BearerToken where