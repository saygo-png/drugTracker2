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
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (TimeZone, diffUTCTime, getCurrentTimeZone, secondsToNominalDiffTime, utcToLocalTime)
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as V
import Path qualified as P
import Path.IO (doesFileExist)
import System.Console.ANSI
import System.Exit (exitFailure)
import TemplateLib
import Text.Time.Pretty (prettyTimeAuto)
import Time
import Types

loadRenderLines :: Bool -> IO (Vector RenderLine)
loadRenderLines detailed = do
  drugData <- loadDrugData
  drugDefs <- loadDrugDefinitions
  now <- getCurrentTime
  lTZ <- getCurrentTimeZone

  let sortedEntries = sortOn (Down . getDate) drugData
      defMap = V.foldr (\dd -> M.insert (getName dd) (getPeriod dd)) M.empty drugDefs
      combinedEntries = V.imapMaybe (makeCombinedEntry defMap sortedEntries) sortedEntries

  if V.null combinedEntries
    then putStrLn "No entries matching existing definitions found!" >> exitFailure
    else pure . V.imap (constructRenderLine lTZ now) $ reverse combinedEntries
  where
    dateStampRel now date =
      let f = if detailed then dayhourTimeFormat now else prettyTimeAuto now
       in (<> ",") . T.pack $ f date

    makeCombinedEntry defMap sortedE i dl = do
      period <- M.lookup (getEntryName dl) defMap
      let isDupe = V.any ((== getEntryName dl) . getEntryName) (V.take i sortedE)
      pure (dl, isDupe, period)

    constructRenderLine localTZ now i (drugLine, old, period) =
      let date = getDate drugLine
          absStamp = toPrettyLocalTime localTZ date
          relStamp = dateStampRel now date
       in RenderLine
            drugLine
            old
            (moreThanNSecondsAgo period date now && not old)
            period
            relStamp
            absStamp
            $ i + 1

getColorizeRG :: IO (Bool -> Text -> Text)
getColorizeRG = (. bool Red Green) . ($ Vivid) <$> getColorize

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

toPrettyLocalTime :: TimeZone -> UTCTime -> Text
toPrettyLocalTime localTZ utcTime =
  let text = utcToLocalTime localTZ utcTime & tshow
   in takeWhile (/= '.') text & dropEnd 3

haveYouRanErr :: IO a
haveYouRanErr = putStrLn "Have you ran \"drug take\"?" >> exitFailure

loadDrugDefinitions :: IO (Vector DrugDefinition)
loadDrugDefinitions = do
  csvFile <- getCsvDrugDefinitions
  getFileState csvFile >>= \case
    FileNotExists -> haveYouRanErr
    FileEmpty -> haveYouRanErr
    FileHasContent -> do
      fileData <- BL8.fromStrict <$> B8.readFile (P.fromAbsFile csvFile)
      case parseCSV fileData of
        Left t -> terror t
        Right v -> pure $ V.nubBy (comparing getName) v
      where
        parseCSV fileData =
          case Cassava.decodeByName @DrugDefinition fileData of
            Left e -> Left $ "Error reading database " <> T.pack e
            Right (_, vec) -> Right vec

loadDrugData :: IO (Vector DrugLine)
loadDrugData = do
  csvFile <- getCsvEntries
  getFileState csvFile >>= \case
    FileNotExists -> haveYouRanErr
    FileEmpty -> haveYouRanErr
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
  exists <- doesFileExist path
  if not exists
    then pure FileNotExists
    else do
      isEmpty <- P.fromAbsFile path & B8.readFile <&> B8.null
      pure $ if isEmpty then FileEmpty else FileHasContent
