module Lib (
  module TemplateLib,
  toPrettyLocalTime,
  getFileState,
  getSafeSetSGRCode,
  getColorize,
  getColorizeRG,
  loadRenderLines,
  loadDrugDefinitions,
  moreThanNSecondsAgo,
  loadDrugData,
  parseEntriesCSV,
) where

import ClassyPrelude
import Types
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTimeZone, secondsToNominalDiffTime, utcToLocalTime)
import Data.Vector qualified as V
import Data.Vector.Algorithms (nubBy)
import Path qualified as P
import Path.IO qualified as PI
import System.Console.ANSI
import System.Exit (exitFailure)
import TemplateLib

loadRenderLines :: IO (Vector RenderLine)
loadRenderLines = do
  drugData <- loadDrugData
  drugDefs <- loadDrugDefinitions
  now <- getCurrentTime

  let sortedEntries = sortOn (Down . getDate) drugData
      lastEntries = combine (markDupesBy ((==) `on` getEntryName) sortedEntries) drugDefs
      finalEntries =
        map
          (\(d, dupe, p) -> RenderLine d dupe (moreThanNSecondsAgo p (getDate d) now && not dupe) p)
          lastEntries
  if V.null lastEntries then putStrLn "No entries matching existing definitions found!" >> exitFailure else pure finalEntries
  where
    markDupesBy :: (a -> a -> Bool) -> Vector a -> Vector (a, Bool)
    markDupesBy eq vec = V.imap checkDuplicate vec
      where
        checkDuplicate i x =
          let isDuplicate = V.any (eq x) (V.take i vec)
           in (x, isDuplicate)
    combine :: Vector (DrugLine, Bool) -> Vector DrugDefinition -> Vector (DrugLine, Bool, Integer)
    combine drugLines drugDefs =
      V.concatMap
        ( \(dl, b) ->
            V.mapMaybe
              ( \dd ->
                  if getEntryName dl == getName dd
                    then Just (dl, b, getPeriod dd)
                    else Nothing
              )
              drugDefs
        )
        drugLines

getColorizeRG :: IO (Bool -> Text -> Text)
getColorizeRG = do
  colorize <- getColorize
  pure $ colorize Vivid . bool Red Green

getColorize :: IO (ColorIntensity -> Color -> Text -> Text)
getColorize = do
  ansiSupport <- hNowSupportsANSI stdout
  pure $
    if ansiSupport
      then \ci c t -> fromString (setSGRCode [SetColor Foreground ci c]) <> t
      else const $ const id

getSafeSetSGRCode :: IO ([SGR] -> Text)
getSafeSetSGRCode = do
  ansiSupport <- hNowSupportsANSI stdout
  pure $
    if ansiSupport
      then fromString . setSGRCode
      else const ""

moreThanNSecondsAgo :: Integer -> UTCTime -> UTCTime -> Bool
moreThanNSecondsAgo secs oldTime now =
  let diff = diffUTCTime now oldTime
      secs' = secondsToNominalDiffTime (fromInteger secs)
   in diff > secs'

toPrettyLocalTime :: UTCTime -> IO Text
toPrettyLocalTime utcTime = do
  localTZ <- getCurrentTimeZone
  let text = utcToLocalTime localTZ utcTime & tshow
  takeWhile (/= '.') text & dropEnd 3 & pure

loadDrugDefinitions :: IO (Vector DrugDefinition)
loadDrugDefinitions = do
  csvFile <- getCsvDrugDefinitions
  getFileState csvFile >>= \case
    FileNotExists -> putStrLn "Have you ran \"drug create\"?" >> exitFailure
    FileEmpty -> putStrLn "Have you ran \"drug create\"?" >> exitFailure
    FileHasContent -> do
      fileData <- BL8.fromStrict <$> B8.readFile (P.fromAbsFile csvFile)
      case parseCSV fileData of
        Left t -> terror t
        Right v -> pure $ nubBy (comparing getName) v
      where
        parseCSV fileData =
          case Cassava.decodeByName @DrugDefinition fileData of
            Left e -> Left $ "Error reading database " <> T.pack e
            Right (_, vec) -> Right vec

loadDrugData :: IO (Vector DrugLine)
loadDrugData = do
  csvFile <- getCsvEntries
  getFileState csvFile >>= \case
    FileNotExists -> putStrLn "Have you ran \"drug take\"?" >> exitFailure
    FileEmpty -> putStrLn "Have you ran \"drug take\"?" >> exitFailure
    FileHasContent -> do
      fileData <- BL8.fromStrict <$> B8.readFile (P.fromAbsFile csvFile)
      case parseEntriesCSV fileData of
        Left t -> terror t
        Right v -> pure v

parseEntriesCSV :: BL8.ByteString -> Either Text (Vector DrugLine)
parseEntriesCSV fileData =
  case Cassava.decodeByName @DrugLine fileData of
    Left e -> Left $ "Error reading database " <> T.pack e
    Right (_, vec) -> Right vec

getFileState :: P.Path P.Abs P.File -> IO FileState
getFileState path = do
  exists <- PI.doesFileExist path
  if not exists
    then pure FileNotExists
    else do
      isEmpty <- P.fromAbsFile path & BS8.readFile <&> BS8.null
      pure $ if isEmpty then FileEmpty else FileHasContent
