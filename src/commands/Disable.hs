module Disable (disable) where

import ClassyPrelude
import Data.ByteString.Char8 qualified as B8
import Data.Csv qualified as Cassava
import Lib
import Path qualified as P
import System.Exit (exitFailure)
import Types

disable :: IO ()
disable = do
  (defs, output) <- loadDrugDefinitions
  drugName <- getDrugDef
  newRecords <- mapM (disableDrugDef drugName) defs

  let fOutput = P.fromAbsFile output
      header = definitionsToHeader csvDefinitionsHT
      dataForWrite = Cassava.encodeByName header (toList newRecords)

  B8.writeFile fOutput $ toStrict dataForWrite

disableDrugDef :: Text -> DrugDefinition -> IO DrugDefinition
disableDrugDef t d
  | m $ getReminding d = putStrLn ("Drug \"" <> t <> "\" has been disabled") >> pure d{getReminding = False}
  | m . not $ getReminding d = putStrLn ("Drug \"" <> t <> "\" is already disabled") >> exitFailure
  | otherwise = pure d
  where
    m predicate = getName d == t && predicate
