{-# LANGUAGE MultiWayIf #-}

module EnableDisable (enable, disable) where

import ClassyPrelude
import Data.ByteString.Char8 qualified as B8
import Data.Csv qualified as Cassava
import Data.Vector qualified as V
import Lib
import Path qualified as P
import System.Console.ANSI
import System.Exit (exitFailure)
import Types qualified as DD (DrugDefinition(..))
import Types

enable, disable :: IO ()
enable = mkToggle True "enabled"
disable = mkToggle False "disabled"

data RecordState where
  RecordChanged :: Text -> RecordState
  RecordAlreadySet :: Text -> RecordState
  RecordNotMatched :: Text -> RecordState
  deriving stock (Eq)

updateDrug :: Bool -> Text -> Text -> DrugDefinition -> (DrugDefinition, RecordState)
updateDrug targetState actionText inputName drug
  | not match = (drug, RecordNotMatched $ unwords ["Drug", quote inputName, "is not", quote drug.name])
  | isEnabled == targetState = (drug, RecordAlreadySet $ msg "is already")
  | otherwise = (drug{DD.reminding = targetState}, RecordChanged $ msg "has been")
  where
    match = drug.name == inputName
    isEnabled = drug.reminding
    msg verb = unwords ["Drug", quote inputName, verb, actionText]

mkToggle :: Bool -> Text -> IO ()
mkToggle targetState actionText = do
  (defs, output) <- loadDrugDefinitions
  drugName <- getDrugNameFromInput
  newRecords <- processRecords $ map (updateDrug targetState actionText drugName) defs
  let header = definitionsToHeader csvDefinitionsHT
      dataForWrite = Cassava.encodeByName header (toList newRecords)
  B8.writeFile (P.fromAbsFile output) $ toStrict dataForWrite
  where
    processRecords :: Vector (DrugDefinition, RecordState) -> IO (Vector DrugDefinition)
    processRecords records = do
      let putRedLn t = getColorize >>= \c -> putStrLn $ c Vivid Red t
          extract f = V.mapMaybe (\case (_, s) -> f s) records
          notMatched = extract $ \case RecordNotMatched msg -> Just msg; _ -> Nothing
          alreadySet = extract $ \case RecordAlreadySet msg -> Just msg; _ -> Nothing
          changed = extract $ \case RecordChanged msg -> Just msg; _ -> Nothing
      if
        | length notMatched == length records ->
            putRedLn "No entries match because:"
              >> mapM_ (putRedLn . ("  " <>)) notMatched
              >> exitFailure
        | not $ null alreadySet -> mapM_ putRedLn alreadySet >> exitFailure
        | not $ null changed -> mapM_ putStrLn changed $> map fst records
        | otherwise -> putRedLn "Entry not matched and not already set." >> exitFailure
