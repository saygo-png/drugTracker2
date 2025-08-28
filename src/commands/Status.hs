module Status (
  status,
) where

import ClassyPrelude
import Lib
import System.Console.ANSI
import Table
import Types

status :: IO ()
status = do
  (dds, _) <- loadDrugDefinitions
  coloredTable <- colorTable colorExample $ plainTable getInfo header dds

  putStrLn coloredTable
  where
    colorExample ListContext{} = White
    colorExample (StatusContext True) = Green
    colorExample (StatusContext False) = Red

    header :: Row
    header = Row $ fromList ["Name", "Frequency", "Reminding"]

    getInfo dd = RenderContext (StatusContext dd.reminding) row
      where
        every p = "Every " <> tshow p <> "s"
        row = SemiRow . map PureCell $ fromList [dd.name, every dd.period, tshow dd.reminding]
