module List (
  listDrugs,
) where

import ClassyPrelude
import Config
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Lib
import Path qualified as P
import Text.Time.Pretty as PT
import Time
import Types

type Row = Vector Text

type TableLocal = Vector Row

listDrugs :: ListArgs -> IO ()
listDrugs args = do
  result <- loadDrugData
  case result of
    Left e -> putStrLn e
    Right d ->
      if null d
        then
          putStrLn "No entries to show!"
        else prettyPrint args d

-- Parsing

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

-- Formatting

takeLast :: (IsSequence seq) => Index seq -> seq -> seq
takeLast i l = reverse l & take i & reverse

padRToLen :: Int -> Text -> Text
padRToLen maxLen t = t <> replicate (maxLen - length t) ' '

prettyTable :: TableLocal -> Text
prettyTable t =
  let cols = length <$> t & V.maximum

      columnWidths = V.generate cols colWidth
        where
          colWidth col = V.mapMaybe (\row -> length <$> (row V.!? col)) t & V.maximum

      formatRow row = V.imap formatEntry row & customJoin
        where
          formatEntry i = padRToLen (columnWidths V.! i)
          customJoin list = intercalate colText (take 3 list) ++ " " ++ unwords (drop 3 list)

      header :: TableLocal
      header =
        fromList [a <> b, mkBreak maxRowLength]
        where
          a = fromList ["Nr"]
          b = TE.decodeUtf8 <$> csvHeader
          maxRowLength = fmap length (formatRow <$> t) & V.maximum
   in intercalate "\n" (formatRow <$> (header <> t))

mkBreak :: Int -> Row
mkBreak i = fromList [T.replicate i rowText]

prettyPrint :: ListArgs -> Vector DrugLine -> IO ()
prettyPrint args vec = do
  nl <- niceLines (getDetailed args) vec
  putStrLn . prettyTable $ takeOrAll (getLines args) nl
  where
    takeOrAll la nl = case la of
      LinesAll -> nl
      LinesInt n -> takeLast n nl

-- Data transformation

niceLines :: Bool -> Vector DrugLine -> IO TableLocal
niceLines detailed vec = do
  let nums = V.generate (length vec) (\x -> tshow $ x + 1)
  let names = fromMaybe "DRUG NAME MISSING" . drugData <$> vec
  absDates <- traverse dateStampAbs vec
  relDates <- traverse (dateStampRel detailed) vec
  pure $
    zipWith4
      (\num name dateRel dateAbs -> fromList [num, name, dateRel, dateAbs])
      nums
      names
      relDates
      absDates

dateStampRel :: Bool -> DrugLine -> IO Text
dateStampRel detailed dl
  | detailed = do (<> ",") . T.pack <$> dayhourTimeFormat oldDate
  | otherwise = do (<> ",") . T.pack <$> prettyTimeAutoFromNow oldDate
  where
    oldDate = dateData dl

dateStampAbs :: DrugLine -> IO Text
dateStampAbs dl = do dateData dl & toPrettyLocalTime
