{-# LANGUAGE TemplateHaskell #-}

module TemplateLib (
  getCsvDrugDefinitions,
  getCsvEntries,
  getDataDir,
) where

import ClassyPrelude
import Path qualified as P
import Path.IO qualified as PI

getDataDir :: IO (P.Path P.Abs P.Dir)
getDataDir = PI.getXdgDir PI.XdgData $ Just $(P.mkRelDir "drug2")

getCsvDrugDefinitions :: IO (P.Path P.Abs P.File)
getCsvDrugDefinitions = getFileInDataDir $(P.mkRelFile "drugDefinitions.csv")

getCsvEntries :: IO (P.Path P.Abs P.File)
getCsvEntries = getFileInDataDir $(P.mkRelFile "data.csv")

getFileInDataDir :: P.Path P.Rel t -> IO (P.Path P.Abs t)
getFileInDataDir file = flip (P.</>) file <$> getDataDir
