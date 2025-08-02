module Lib (
  toPrettyLocalTime,
  loadDrugData,
  parseCSV,
) where

import ClassyPrelude
import Config
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Text qualified as T
import Data.Time (getCurrentTimeZone, utcToLocalTime)
import Path qualified as P
import Types

toPrettyLocalTime :: UTCTime -> IO Text
toPrettyLocalTime utcTime = do
  localTZ <- getCurrentTimeZone
  let text = utcToLocalTime localTZ utcTime & tshow
  takeWhile (/= '.') text & dropEnd 3 & pure

loadDrugData :: IO (Either Text (Vector DrugLine))
loadDrugData = do
  csvFile <- getCsvFile
  fileResult <- try $ BL8.readFile $ P.fromAbsFile csvFile
  return $ case fileResult of
    Left (e :: IOError) -> Left $ T.pack (displayException e) <> "\nHave you ran \"drug take DRUG_NAME\"?"
    Right fileData -> parseCSV fileData

parseCSV :: BL8.ByteString -> Either Text (Vector DrugLine)
parseCSV fileData =
  case Cassava.decodeByName @DrugLine fileData of
    Left e -> Left $ "Error reading database " <> T.pack e
    Right (_, vec) -> Right vec
