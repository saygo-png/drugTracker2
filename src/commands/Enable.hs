module Enable (enable) where

import ClassyPrelude
import Data.ByteString.Char8 qualified as B8
import Data.Csv qualified as Cassava
import Lib
import Path qualified as P
import System.Exit (exitFailure)
import Types

enable :: IO ()
enable = do
  (defs, output) <- loadDrugDefinitions
  drugName <- getDrugDef
  newRecords <- mapM (enableDrugDef drugName) defs

  let fOutput = P.fromAbsFile output
      header = definitionsToHeader csvDefinitionsHT
      dataForWrite = Cassava.encodeByName header (toList newRecords)

  B8.writeFile fOutput $ toStrict dataForWrite

enableDrugDef :: Text -> DrugDefinition -> IO DrugDefinition
enableDrugDef t d
  | m . not $ getReminding d = putStrLn ("Drug \"" <> t <> "\" has been enabled") >> pure d{getReminding = True}
  | m $ getReminding d = putStrLn ("Drug \"" <> t <> "\" is already enabled") >> exitFailure
  | otherwise = pure d
  where
    m predicate = getName d == t && predicate
