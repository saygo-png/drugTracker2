module EnableDisable (enable, disable) where

import ClassyPrelude
import Data.ByteString.Char8 qualified as B8
import Data.Csv qualified as Cassava
import Lib
import Path qualified as P
import System.Exit (exitFailure)
import Types

enable :: IO ()
enable = mkDisableEnable (updateDrug True "enabled")

disable :: IO ()
disable = mkDisableEnable (updateDrug False "disabled")

updateDrug :: Bool -> Text -> Text -> DrugDefinition -> IO DrugDefinition
updateDrug targetState actionText drugName drug
  | match && isEnabled /= targetState = msg "has been" $> drug{getReminding = targetState}
  | match && isEnabled == targetState = msg "is already" >> exitFailure
  | otherwise = pure drug
  where
    match = getName drug == drugName
    isEnabled = getReminding drug
    msg verb = putStrLn $ unwords ["Drug", "\"" <> drugName <> "\"", verb, actionText]

mkDisableEnable :: (Text -> DrugDefinition -> IO DrugDefinition) -> IO ()
mkDisableEnable f = do
  (defs, output) <- loadDrugDefinitions
  drugName <- getDrugDef
  newRecords <- mapM (f drugName) defs
  let fOutput = P.fromAbsFile output
      header = definitionsToHeader csvDefinitionsHT
      dataForWrite = Cassava.encodeByName header (toList newRecords)
  B8.writeFile fOutput $ toStrict dataForWrite
