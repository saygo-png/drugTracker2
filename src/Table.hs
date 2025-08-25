module Table (
  plainTable,
  colorTableWithJoin,
  colorTable,
  Table (Table, heading, rows),
  ContextType (StatusContext, ListContext),
  RenderContext (RenderContext),
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

data RenderContext a = RenderContext ContextType a
  deriving (Functor)

type IsReminding = Bool

type Row = Vector Text

type Header = Vector Text

data ContextType = StatusContext IsReminding | ListContext IsReminding IsOld IsMissed

data Table = Table
  { heading :: Vector (Vector Text)
  , rows :: Vector (RenderContext (Vector Text))
  }

extractRow :: RenderContext Row -> Row
extractRow (RenderContext _ row) = row

colorTableWithJoin :: (Bool -> Row -> Text) -> (ContextType -> Color) -> Table -> IO Text
colorTableWithJoin customJoin f table = do
  colorize <- getColorize

  let coloredTable = map (customJoin False) table.heading <> map colorRow table.rows
      colorRow r = customJoin True $ colorize Vivid (color r) <$> extractRow r

  pure $ intercalate "\n" coloredTable
  where
    color (RenderContext a _) = f a

colorTable :: (ContextType -> Color) -> Table -> IO Text
colorTable f table = do
  safeSetSGRCode <- getSafeSetSGRCode

  let customJoin :: Bool -> Row -> Text
      customJoin doColor cell =
        let delimiter =
              if doColor
                then safeSetSGRCode [SetDefaultColor Foreground] <> config.columnString
                else config.columnString
         in intercalate delimiter cell

  colorTableWithJoin customJoin f table

plainTable :: (a -> RenderContext Row) -> Header -> Vector a -> Table
plainTable mkRow header elems =
  let
    rows = map mkRow elems
    allRows = header `cons` map extractRow rows

    padRow = V.imap (padFromRows allRows)
    joinCols = intercalate config.columnString

    separator = singleton $ T.replicate separatorLength config.rowString
      where
        separatorLength = on max length (joinCols headerPadded) firstRowFormatted
        firstRowFormatted = case headMay (map extractRow rows) of
          Just row -> joinCols (padRow row)
          Nothing -> ""

    headerPadded = padRow header
    paddedRows = map (map padRow) rows

    heading = cons headerPadded $ singleton separator
   in
    Table heading paddedRows
  where
    padFromRows :: (MonoFoldable a) => Vector (Vector a) -> Int -> Text -> Text
    padFromRows rows col = T.justifyLeft (fromMaybe 0 $ columnWidths !? col) ' '
      where
        columnWidths = V.generate numCols colWidth
          where
            numCols = fromMaybe 0 . maximumMay $ length <$> rows
            colWidth c =
              V.mapMaybe (\x -> length <$> (x !? c)) rows
                & maximumMay
                & fromMaybe 0
