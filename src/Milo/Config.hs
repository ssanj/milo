{-# LANGUAGE OverloadedStrings #-}

module Milo.Config (getAppEnv) where

import qualified Milo.Config.Model as M
import qualified Milo.Config.YamlConfig as YC

getAppEnv :: IO M.Env
getAppEnv = (M.loadConfig YC.getConfigProvider) "milo"
