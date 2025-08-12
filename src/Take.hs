module Take (
  takeDrug,
) where

import ClassyPrelude
import Types
import Config
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time qualified as TI
import Lib
import Path qualified as P
import Path.IO qualified as PI
import System.Exit (exitFailure)
import System.Process.Typed qualified as S
import Text.Printf (printf)

takeDrug :: IO ()
takeDrug = do
  drug <- liftA2 DrugLine getDrugDef TI.getCurrentTime

  output <- getCsvEntries
  let fOutput = P.fromAbsFile output

  getFileState output >>= \case
    FileNotExists -> do
      PI.createDirIfMissing True =<< getDataDir
      writeWithHeader drug fOutput
    FileEmpty -> writeWithHeader drug fOutput
    FileHasContent -> appendWithoutHeader drug fOutput

getDrugDef :: IO Text
getDrugDef = do
  defs <- loadDrugDefinitions
  result <- runFuzzyFinder fuzzyFinder . intercalate "\n" . sort $ fmap getName defs
  maybe (putStrLn "Invalid input" >> exitFailure) pure result
  where
    runFuzzyFinder :: String -> Text -> IO (Maybe Text)
    runFuzzyFinder finder input = do
      (exitCode, output, _) <- S.readProcess . pipeIn input $ S.proc finder []

      case exitCode of
        S.ExitSuccess -> decode output
        _ -> pure Nothing
      where
        pipeIn = S.setStdin . S.byteStringInput . BL8.fromStrict . T.encodeUtf8
        decode = pure . Just . T.toStrict . T.strip . TL.decodeUtf8

wroteInfo :: DrugLine -> IO ()
wroteInfo DrugLine{..} =
  printf "Took \"%s\" on %s\n" getEntryName =<< toPrettyLocalTime getDate

writeWithHeader :: DrugLine -> FilePath -> IO ()
writeWithHeader drug output = do
  let dataForWrite = Cassava.encodeByName (entriesToHeader csvEntriesHT) [drug]
  BL8.writeFile output dataForWrite
  wroteInfo drug

appendWithoutHeader :: DrugLine -> FilePath -> IO ()
appendWithoutHeader drug output = do
  let dataForWrite = Cassava.encode [drug]
  BL8.appendFile output dataForWrite
  wroteInfo drug
