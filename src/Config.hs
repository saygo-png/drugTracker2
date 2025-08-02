{-# LANGUAGE TemplateHaskell #-}

module Config (getCsvFile, remindPeriod, getDataDir, csvHeader, colText, rowText) where

import ClassyPrelude
import Data.Csv qualified as C
import Data.Vector qualified as Vector
import Path qualified as P
import Path.IO qualified as PI
import Data.Fixed (Pico)

getCsvFile :: IO (P.Path P.Abs P.File)
getCsvFile = getFileInDataDir $(P.mkRelFile "data.csv")
  where
    getFileInDataDir file = flip (P.</>) file <$> getDataDir

getDataDir :: IO (P.Path P.Abs P.Dir)
getDataDir = PI.getXdgDir PI.XdgData $ Just $(P.mkRelDir "drug2")

colText :: Text
colText = " | "

remindPeriod :: Pico
remindPeriod = day
  where
      day = 86400


rowText :: Text
rowText = "-"

csvHeader :: C.Header
csvHeader =
  Vector.fromList
    [ "Name"
    , "Date"
    ]
