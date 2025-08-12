{-# LANGUAGE TemplateHaskell #-}

module LoadConfig (config) where

import Types
import ParseConfig

config :: Config
config = $(loadConfig)
