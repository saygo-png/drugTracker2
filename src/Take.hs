module Take (
  takeDrug,
) where

import ClassyPrelude
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (getCurrentTimeZone)
import Lib
import LoadConfig
import Path qualified as P
import Path.IO (createDirIfMissing)
import System.Exit (exitFailure)
import System.Process.Typed qualified as S
import Text.Printf (printf)
import Types

takeDrug :: IO ()
takeDrug = do
  drug <- liftA2 DrugLine getDrugDef getCurrentTime

  output <- getCsvEntries
  let fOutput = P.fromAbsFile output

  getFileState output >>= \case
    FileNotExists -> do
      createDirIfMissing True =<< getDataDir
      writeWithHeader drug fOutput
    FileEmpty -> writeWithHeader drug fOutput
    FileHasContent -> appendWithoutHeader drug fOutput

getDrugDef :: IO Text
getDrugDef = do
  defs <- loadDrugDefinitions
  result <- runPicker (picker config) . intercalate "\n" . sort $ getName <$> defs
  maybe (putStrLn "Invalid input" >> exitFailure) pure result
  where
    runPicker finder input = do
      (exitCode, output, _) <- S.readProcess . pipeIn input $ S.proc finder []
      case exitCode of
        S.ExitSuccess -> decode output
        _ -> pure Nothing
      where
        pipeIn = S.setStdin . S.byteStringInput . BL8.fromStrict . T.encodeUtf8
        decode = pure . Just . T.toStrict . T.strip . TL.decodeUtf8

wroteInfo :: DrugLine -> IO ()
wroteInfo DrugLine{..} = do
  lTZ <- getCurrentTimeZone
  printf "Took \"%s\" on %s\n" getEntryName $ toPrettyLocalTime lTZ getDate

writeWithHeader :: DrugLine -> FilePath -> IO ()
writeWithHeader drug output =
  let dataForWrite = Cassava.encodeByName (entriesToHeader csvEntriesHT) [drug]
   in BL8.writeFile output dataForWrite >> wroteInfo drug

appendWithoutHeader :: DrugLine -> FilePath -> IO ()
appendWithoutHeader drug output =
  let dataForWrite = Cassava.encode [drug]
   in BL8.appendFile output dataForWrite >> wroteInfo drug
