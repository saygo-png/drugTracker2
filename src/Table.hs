-- {-# LANGUAGE OverloadedRecordDot #-}
module Table (
  plainTable,
  colorTable,
  Cell (PureCell, SemiCell),
  Row (Row),
  SemiRow (SemiRow),
  JointRow (JointRow),
  Table (Table, header, rows),
  ContextType (StatusContext, ListContext),
  RenderContext (RenderContext),
) where

import ClassyPrelude
import Data.List.NonEmpty qualified as N
import Data.Text qualified as T
import Data.Vector qualified as V
import Lib
import LoadConfig
import System.Console.ANSI
import Types

type role RenderContext nominal

data RenderContext a = RenderContext ContextType a
  deriving stock (Functor)

type IsReminding = Bool

data Cell where
  PureCell :: Text -> Cell
  SemiCell :: (Vector Text) -> Cell

newtype SemiRow = SemiRow {semiRow :: Vector Cell}

newtype Row = Row {row :: Vector Text}

newtype JointRow = JointRow {jointRow :: Text}

data ContextType where
  StatusContext :: IsReminding -> ContextType
  ListContext :: IsReminding -> IsOld -> IsMissed -> ContextType

data Table = Table
  { header :: Row
  , rows :: Vector (RenderContext Row)
  }

extractRow :: RenderContext Row -> Row
extractRow (RenderContext _ row) = row

colorTable :: (ContextType -> Color) -> Table -> IO Text
colorTable f t = do
  colorize <- getColorize
  safeSetSGRCode <- getSafeSetSGRCode

  let coloredTable =
        case N.nonEmpty . unpack $ config.rowString of
          Nothing -> addHeader coloredRows
          Just rs -> addHeader $ cons (separator rs) coloredRows
        where
          addHeader = cons $ joinPlain t.header

          separator s = JointRow . pack . N.take sepLen $ N.cycle s
            where
              sepLen = length . (.jointRow) $ joinPlain t.header

          coloredRows = map colorJoinRow t.rows
            where
              colorJoinRow r = joinColor . Row $ color <$> rows
                where
                  rows = (extractRow r).row
                  color = colorize Vivid ((\(RenderContext a _) -> f a) r)

      joinColor c = JointRow . intercalate delimiter $ c.row
        where
          delimiter = safeSetSGRCode [SetDefaultColor Foreground] <> config.columnString

      joinPlain :: Row -> JointRow
      joinPlain c = JointRow $ intercalate config.columnString c.row

  pure . intercalate "\n" . map (.jointRow) $ coloredTable

plainTable :: (a -> RenderContext SemiRow) -> Row -> Vector a -> Table
plainTable mkRow header elems = Table paddedHeader paddedRows
  where
    fakeRows = cons header $ map extractRow semisProcessed

    paddedHeader = under (V.imap pad) header
    paddedRows = map (map (under $ V.imap pad)) semisProcessed

    pad :: Int -> Text -> Text
    pad = padCell2 fakeRows

    semisProcessed :: Vector (RenderContext Row)
    semisProcessed = map pureRow <$> inputRows
      where
        inputRows :: Vector (RenderContext SemiRow)
        inputRows = map mkRow elems

        pureRow :: SemiRow -> Row
        pureRow = dealWithSemis mkRow elems

padCell2 :: Vector Row -> Int -> Text -> Text
padCell2 rows idx = T.justifyLeft (columnWidths rows V.! idx) ' '
  where
    columnWidths :: Vector Row -> Vector Int
    columnWidths ar = V.generate (maxCols ar) (maxLengthInCol ar)
      where
        maxCols :: Vector Row -> Int
        maxCols rows' = fromMaybe err . maximumMay $ map (length . (.row)) rows'
          where
            err = terror "Rows is somehow empty"

        maxLengthInCol :: Vector Row -> Int -> Int
        maxLengthInCol allRows col = V.maximum $ map (cellLength col) ((.row) <$> allRows)
          where
            cellLength col' cell' = T.length $ cell' !?! (col', err)
              where
                err = terror "Wrong numbers of columns"

dealWithSemis :: (a -> RenderContext SemiRow) -> Vector a -> SemiRow -> Row
dealWithSemis mkRow elems s = Row $ map f s.semiRow
  where
    f :: Cell -> Text
    f (PureCell t) = t
    f (SemiCell v) = unwords $ V.imap (padCell2 allRows) v
      where
        allRows = map convertSemis base
          where
            base :: Vector SemiRow
            base = map ((\(RenderContext _ sr) -> sr) . mkRow) elems

            onlySemiCells :: Cell -> Bool
            onlySemiCells (PureCell _) = False
            onlySemiCells (SemiCell _) = True

            convertSemis :: SemiRow -> Row
            convertSemis r = Row $ concatMap semiToCells onlySemis
              where
                onlySemis :: Vector Cell
                onlySemis = V.filter onlySemiCells r.semiRow

                semiToCells (PureCell _) = terror "applied to a normal cell"
                semiToCells (SemiCell vv) = vv
