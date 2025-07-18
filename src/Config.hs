module Config (outputPath, csvHeader) where

import ClassyPrelude
import Data.Csv
import Data.Vector qualified as Vector
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)

-- Path relative to $XDG_DATA_HOME
outputPath :: IO FilePath
outputPath = getXdgDirectory XdgData "drug2/data.csv"

csvHeader :: Header
csvHeader =
  Vector.fromList
    [ "drug"
    , "date"
    ]
