module Take (
  takeDrug,
) where

import ClassyPrelude
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Time (getCurrentTimeZone)
import Lib
import Path qualified as P
import Path.IO (createDirIfMissing)
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
