{-# LANGUAGE TemplateHaskell #-}

module ParseConfig (
  loadConfig,
) where

import ClassyPrelude hiding (lift)
import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Syntax qualified as TH
import Path (mkRelFile)
import Path.IO (doesFileExist)
import Types

defaultConfig :: Config
defaultConfig =
  Config
    { columnString = " | " -- Text for drawing columns
    , rowString = "-" -- Text for separating the header from content
    , picker = "fzf" -- Name of the picker binary to use for different selection menus
    }

loadConfig :: Q Exp
loadConfig = do
  exists <- runIO $ doesFileExist $(mkRelFile "config.json")
  if exists
    then do
      jsonContent <- runIO $ readFile "config.json"
      case eitherDecodeStrict jsonContent of
        Left err -> error $ "COMPILE-TIME ERROR: Invalid JSON: " <> err
        Right (cfg :: Config) -> TH.lift cfg
    else do
      reportWarning "config.json not found, using default configuration"
      TH.lift defaultConfig
