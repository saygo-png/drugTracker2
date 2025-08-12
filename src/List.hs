module List (
  listDrugs,
) where

import ClassyPrelude
import Data.Function ((&))
import Data.Text qualified as T
import Data.Vector ((!?))
import Data.Vector qualified as V
import Lib
import LoadConfig
import System.Console.ANSI
import Types

listDrugs :: ListArgs -> IO ()
listDrugs ListArgs{..} = do
  nl <- loadRenderLines getDetailed
  putStrLn =<< prettyTable config (handleArgs getLines getUniques nl)
  where
    handleArgs :: LinesArg -> Bool -> Vector RenderLine -> Vector RenderLine
    handleArgs la unique nl =
      if unique
        then filter (not . getIsOld) nl
        else case la of
          LinesAll -> nl
          LinesInt n -> takeLast n nl
            where
              takeLast i = reverse . take i . reverse

prettyTable :: Config -> Vector RenderLine -> IO Text
prettyTable Config{..} rls = do
  colorize <- getColorize
  safeSetSGRCode <- getSafeSetSGRCode
  let
    rows = mkRowVec <$> rls
    numCols = fromMaybe 0 . maximumMay $ length <$> rows
    columnWidths = V.generate numCols colWidth
      where
        colWidth col =
          V.mapMaybe (\x -> length <$> (x !? col)) rows
            & maximumMay
            & fromMaybe 0

    pad col = T.justifyLeft (fromMaybe 0 $ columnWidths !? col) ' '

    formatColoredCell isOld isMissed colIndex text =
      let cellColor
            | isOld = Green
            | isMissed = Red
            | not isMissed = Green
            | otherwise = White
       in pad colIndex text & colorize Vivid cellColor

    header = V.imap pad (fromList ["Nr", "Name", "Date"]) & intercalate columnString
    separator = T.replicate separatorLength rowString
      where
        firstRowFormatted = case headMay rls of
          Just rl -> customJoin False . V.imap pad $ mkRowVec rl
          Nothing -> ""
        separatorLength = on max length firstRowFormatted header

    formatRow (row, isOld, isMissed) =
      V.imap (formatColoredCell isOld isMissed) row & customJoin True
    customJoin useColor cells =
      let delimiter =
            if useColor
              then safeSetSGRCode [SetDefaultColor Foreground] <> columnString
              else columnString
       in intercalate delimiter (take 3 cells) <> " " <> unwords (drop 3 cells)

  pure . intercalate "\n" $ header `cons` (separator `cons` map formatRow (mkRow <$> rls))
  where
    mkRow rl = (mkRowVec rl, getIsOld rl, getIsMissed rl)
    mkRowVec RenderLine{..} =
      fromList [tshow getIndex, getEntryName getDrugLine, getDateRel, getDateAbs]
