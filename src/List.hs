module List (
  listDrugs,
) where

import ClassyPrelude
import Config
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Time.Pretty
import Types

loadDrugData :: IO (Either Text (Vector DrugLine))
loadDrugData = do
  fileResult <- try $ BL8.readFile =<< outputPath
  return $ case fileResult of
    Left (e :: IOError) -> Left $ T.pack (displayException e) <> "\nHave you ran \"drug take DRUG_NAME\"?"
    Right fileData -> parseCSV fileData

parseCSV :: BL8.ByteString -> Either Text (Vector DrugLine)
parseCSV fileData =
  case Cassava.decodeByName @DrugLine fileData of
    Left e -> Left $ "Error reading database " <> T.pack e
    Right (_, vec) -> Right vec

listDrugs :: IO ()
listDrugs = do
  result <- loadDrugData
  case result of
    Left e -> putStrLn e
    Right d ->
      if null d
        then
          putStrLn "No entries to show!"
        else prettyPrint d

takeLast :: (IsSequence seq) => Index seq -> seq -> seq
takeLast i l = reverse l & take i & reverse

type Row = Vector Text

type Table = Vector Row

padRToLen :: Int -> Text -> Text
padRToLen maxLen t = t <> replicate (maxLen - length t) ' '

prettyTable :: Table -> Text
prettyTable t =
  let cols = length <$> t & V.maximum

      columnWidths = V.generate cols colWidth
        where
          colWidth col = V.mapMaybe (\row -> length <$> (row V.!? col)) t & V.maximum

      formatRow row = V.imap formatEntry row & intercalate " | "
        where
          formatEntry i = padRToLen (columnWidths V.! i)
   in formatRow <$> t & intercalate "\n"

prettyPrint :: Vector DrugLine -> IO ()
prettyPrint vec = do
  nl <- niceLines vec
  putStrLn . prettyTable $ takeLast 14 nl

niceLines :: Vector DrugLine -> IO Table
niceLines vec = do
  let nums = V.generate (length vec) (\x -> tshow $ x + 1)
  let names = fromMaybe "DRUG NAME MISSING" . drugData <$> vec
  dates <- traverse dateStamp vec
  return $ zipWith3 (\num name date -> fromList [num, name, date]) nums names dates

dateStamp :: DrugLine -> IO Text
dateStamp dl = do
  let date = dateData dl
  let absTime = T.dropEnd 3 $ takeWhile (/= '.') . tshow $ date
  relTime <- T.pack <$> prettyTimeAutoFromNow date
  return $ relTime <> ", " <> absTime
