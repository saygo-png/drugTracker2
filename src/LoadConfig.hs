{-# LANGUAGE TemplateHaskell #-}

module LoadConfig (config) where

import ParseConfig
import Types

config :: Config
config = $(loadConfig)
