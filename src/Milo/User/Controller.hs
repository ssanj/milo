{-# LANGUAGE OverloadedStrings #-}

module Milo.User.Controller (userListAction) where

import Milo.Model

import Milo.Config.Model (Env)
import Milo.User.Service (getUsers)

import qualified Network.HTTP.Client         as Client
import qualified Data.Text                   as T

-- The type of this action is different to the usual TweetResultIO because it will not be displayed
-- but used in conjunction with another service
userListAction :: Env -> Client.Manager -> [EntityId] -> IO (Either T.Text [TwitterUser])
userListAction = getUsers  