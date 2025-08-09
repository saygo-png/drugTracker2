module List (
  listDrugs,
) where

import ClassyPrelude
import ConfigAndTypes
import Data.Function ((&))
import Data.Text qualified as T
import Data.Vector ((!?))
import Data.Vector qualified as V
import Lib
import System.Console.ANSI
import Text.Time.Pretty as PT
import Time

type Row = Vector Text

listDrugs :: ListArgs -> IO ()
listDrugs args = prettyPrint args . reverse =<< loadRenderLines

prettyTable :: Vector (Row, Bool, Bool) -> IO Text
prettyTable table = do
  colorize <- getColorize
  safeSetSGRCode <- getSafeSetSGRCode
  let extractRows (rows, _, _) = rows
      numCols = fromMaybe 0 . maximumMay $ length . extractRows <$> table
      pad col = T.justifyLeft (fromMaybe 0 $ columnWidths !? col) ' '
      columnWidths = V.generate numCols colWidth
        where
          colWidth col =
            V.mapMaybe (\(row, _, _) -> length <$> (row !? col)) table
              & maximumMay
              & fromMaybe 0

      formatColoredCell isOld isMissed colIndex text =
        let cellColor
              | isOld = Green
              | isMissed = Red
              | not isMissed = Green
              | otherwise = White
         in pad colIndex text & colorize Vivid cellColor

      header = V.imap pad (fromList ["Nr", "Name", "Date"]) & intercalate colText
      separator = T.replicate separatorLength rowText
        where
          firstRowFormatted = case headMay table of
            Just (row, _, _) -> customJoin False $ V.imap pad row
            Nothing -> ""
          separatorLength = max (length firstRowFormatted) (length header)

      formatRow (row, isOld, isMissed) =
        V.imap (formatColoredCell isOld isMissed) row & customJoin True
      customJoin useColor cells =
        let delimiter =
              if useColor
                then safeSetSGRCode [SetDefaultColor Foreground] <> colText
                else colText
         in intercalate delimiter (take 3 cells) <> " " <> unwords (drop 3 cells)

  pure . intercalate "\n" $ header `cons` (separator `cons` map formatRow table)

prettyPrint :: ListArgs -> Vector RenderLine -> IO ()
prettyPrint args vec = do
  nl <- niceLines (getDetailed args)
  putStrLn =<< prettyTable (takeOrAll (getLines args) nl)
  where
    dateStampAbs dl = do getDate dl & toPrettyLocalTime
    dateStampRel detailed dl
      | detailed = do (<> ",") . T.pack <$> dayhourTimeFormat oldDate
      | otherwise = do (<> ",") . T.pack <$> prettyTimeAutoFromNow oldDate
      where
        oldDate = getDate dl

    takeOrAll la nl = case la of
      LinesAll -> nl
      LinesInt n -> takeLast n nl
        where
          takeLast :: (IsSequence seq) => Index seq -> seq -> seq
          takeLast i l = reverse l & take i & reverse

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
