module Take (
  takeDrug,
) where

import ClassyPrelude
import Config
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Path qualified as P
import Path.IO qualified as PI
import Text.Printf (printf)
import Types

takeDrug :: Text -> IO ()
takeDrug drugName = do
  output <- getCsvFile
  filestate <- getFileState output
  drug <- DrugLine (Just drugName) <$> getCurrentTime

  let fOutput = P.fromAbsFile output

  case filestate of
    FileNotExists -> do
      PI.createDirIfMissing True =<< getDataDir
      writeWithHeader drug fOutput
    FileEmpty -> writeWithHeader drug fOutput
    FileHasContent -> appendWithoutHeader drug fOutput

getFileState :: P.Path P.Abs P.File -> IO FileState
getFileState path = do
  exists <- PI.doesFileExist path
  if not exists
    then pure FileNotExists
    else do
      isEmpty <- P.fromAbsFile path & BS8.readFile <&> BS8.null
      pure $ if isEmpty then FileEmpty else FileHasContent

wroteInfo :: DrugLine -> IO ()
wroteInfo i = do
  let x = drugData i
  let date = dateData i & tshow & takeWhile (/= '.')
  case x of
    Just name -> printf "Took \"%s\" on %s\n" name date
    Nothing -> error "drugData returned Nothing when it shouldn't have"

writeWithHeader :: DrugLine -> FilePath -> IO ()
writeWithHeader drug output = do
  let dataForWrite = Cassava.encodeByName csvHeader [drug]
  BL8.writeFile output dataForWrite
  wroteInfo drug

appendWithoutHeader :: DrugLine -> FilePath -> IO ()
appendWithoutHeader drug output = do
  let dataForWrite = Cassava.encode [drug]
  BL8.appendFile output dataForWrite
  wroteInfo drug
