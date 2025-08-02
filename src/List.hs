module List (
  listDrugs,
) where

import ClassyPrelude
import ConfigAndTypes
import Data.Function ((&))
import Data.Text qualified as T
import Data.Vector qualified as V
import Lib
import System.Console.ANSI
import Text.Time.Pretty as PT
import Time

type Row = Vector Text

listDrugs :: ListArgs -> IO ()
listDrugs args = prettyPrint args . reverse =<< loadRenderLines

-- Formatting

takeLast :: (IsSequence seq) => Index seq -> seq -> seq
takeLast i l = reverse l & take i & reverse

padRToLen :: Int -> Text -> Text
padRToLen maxLen t = t <> replicate (maxLen - length t) ' '

prettyTable :: Vector (Row, Bool, Bool) -> IO Text
prettyTable t = do
  colorize <- getColorize
  safeSetSGRCode <- getSafeSetSGRCode
  let getRows (rows, _, _) = rows
      cols = (length . getRows <$> t) & V.maximum

      columnWidths = V.generate cols colWidth
        where
          colWidth col = V.mapMaybe (\(row, _, _) -> length <$> (row V.!? col)) t & V.maximum

      formatCell ci = padRToLen (columnWidths V.! ci)
      formatCellColor isOld isMissed ci x = padRToLen (columnWidths V.! ci) x & colorize Vivid color
        where
          color
            | isOld = Green
            | isMissed = Red
            | not isMissed = Green
            | otherwise = White

      formattedHeader = V.imap formatCell (fromList ["Nr", "Name", "Date"]) & intercalate colText
      formattedRows = map formatRow t
      separator = T.replicate i rowText
        where
          i = length $ customJoin False $ V.imap formatCell $ (\(x, _, _) -> x) $ unsafeHead t

      formatRow (row, isOld, isMissed) = V.imap (formatCellColor isOld isMissed) row & customJoin True
      customJoin c list = intercalate colText' (take 3 list) <> " " <> unwords (drop 3 list)
        where
          colText' = bool "" (safeSetSGRCode [SetDefaultColor Foreground]) c <> colText
  pure . intercalate "\n" . cons formattedHeader $ cons separator formattedRows

prettyPrint :: ListArgs -> Vector RenderLine -> IO ()
prettyPrint args vec = do
  nl <- niceLines (getDetailed args)
  putStrLn =<< prettyTable (takeOrAll (getLines args) nl)
  where
    takeOrAll la nl = case la of
      LinesAll -> nl
      LinesInt n -> takeLast n nl

    niceLines :: Bool -> IO (Vector (Row, Bool, Bool))
    niceLines detailed = do
      let drugLines = getDrugLine <$> vec
      relDates <- traverse (dateStampRel detailed) drugLines
      absDates <- traverse dateStampAbs drugLines
      pure $
        zipWith6
          (\num name dateRel dateAbs old missed -> (fromList [num, name, dateRel, dateAbs], old, missed))
          (V.generate (length vec) (\x -> tshow $ x + 1))
          (getEntryName <$> drugLines)
          relDates
          absDates
          (getIsOld <$> vec)
          (getIsMissed <$> vec)

dateStampRel :: Bool -> DrugLine -> IO Text
dateStampRel detailed dl
  | detailed = do (<> ",") . T.pack <$> dayhourTimeFormat oldDate
  | otherwise = do (<> ",") . T.pack <$> prettyTimeAutoFromNow oldDate
  where
    oldDate = getDate dl

dateStampAbs :: DrugLine -> IO Text
dateStampAbs dl = do getDate dl & toPrettyLocalTime
