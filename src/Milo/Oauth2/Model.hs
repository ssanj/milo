{-# LANGUAGE DeriveGeneric #-}

module Milo.Oauth2.Model (OAuth2ClientToken(..), BearerToken(..)) where

import Milo.Config.Model (ClientKey)
import Milo.Config.Model (ClientSecret)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data OAuth2ClientToken = 
  OAuth2ClientToken { key :: ClientKey, secret :: ClientSecret } deriving (Show)

data BearerToken = BearerToken { token_type :: !String, access_token :: !String } deriving (Show, Generic)

instance FromJSON BearerToken where