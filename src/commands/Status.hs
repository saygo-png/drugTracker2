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

    header :: Vector Text
    header = fromList ["Name", "Frequency", "Reminding"]

    getInfo dd = RenderContext (StatusContext dd.reminding) vec
      where
        every p = "Every " <> tshow p <> "s"
        vec = fromList [dd.name, every dd.period, tshow dd.reminding]
