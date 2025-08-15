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

    header = fromList ["Name", "Frequency", "Reminding"]

    getInfo DrugDefinition{..} = RenderContext (StatusContext getReminding) vec
      where
        every p = "Every " <> tshow p <> "s"
        vec = fromList [getName, every getPeriod, tshow getReminding]
