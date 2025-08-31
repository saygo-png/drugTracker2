module Table (
  plainTable,
  colorTable,
  Cell (PureCell, SemiCell),
  Row (Row),
  SemiRow (SemiRow),
  Table,
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
          Nothing -> cons (joinPlain t.header) coloredRows
          Just rs -> cons (joinPlain t.header) $ cons (separator rs) coloredRows
        where
          separator s = JointRow . pack . N.take sepLen $ N.cycle s
            where
              sepLen = length . (.jointRow) $ joinPlain t.header

          joinPlain c = JointRow $ intercalate config.columnString c.row

          coloredRows = map colorJoinRow t.rows
            where
              colorJoinRow r = joinColor . Row $ map color (extractRow r).row
                where
                  color = colorize Vivid ((\(RenderContext a _) -> f a) r)
                  joinColor c = JointRow . intercalate delimiter $ c.row
                    where
                      delimiter = safeSetSGRCode [SetDefaultColor Foreground] <> config.columnString

  pure . intercalate "\n" . map (.jointRow) $ coloredTable

plainTable :: (a -> RenderContext SemiRow) -> Row -> Vector a -> Table
plainTable mkRow header elems = Table paddedHeader paddedRows
  where
    paddedHeader = under (V.imap pad) header
    paddedRows = map (map (under $ V.imap pad)) semisProcessed
    pad = padCell fakeRows
      where
        fakeRows = cons header $ map extractRow semisProcessed

    semisProcessed = map dealWithSemis <$> map mkRow elems
      where
        dealWithSemis :: SemiRow -> Row
        dealWithSemis s = Row $ map f s.semiRow
          where
            f :: Cell -> Text
            f (PureCell t) = t
            f (SemiCell v) = unwords $ V.imap (padCell allRows) v
              where
                allRows = map (convertSemis . (\(RenderContext _ sr) -> sr) . mkRow) elems
                convertSemis r = Row $ concatMap semiToCells (V.filter onlySemis r.semiRow)
                  where
                    onlySemis (PureCell _) = False
                    onlySemis (SemiCell _) = True
                    semiToCells (PureCell _) = terror "applied to a normal cell"
                    semiToCells (SemiCell vv) = vv

padCell :: Vector Row -> Int -> Text -> Text
padCell rows idx = T.justifyLeft (columnWidths rows V.! idx) ' '
  where
    columnWidths ar = V.generate (maxCols ar) (maxLengthInCol ar)
      where
        maxCols rows' = fromMaybe err . maximumMay $ map (length . (.row)) rows'
          where
            err = terror "Rows is somehow empty"

        maxLengthInCol allRows col = V.maximum $ map (cellLength col) ((.row) <$> allRows)
          where
            cellLength col' cell = T.length $ cell !?! (col', err)
            err = terror "Wrong numbers of columns"
