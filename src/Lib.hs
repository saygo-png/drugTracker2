module Lib (
  module TemplateLib,
  toPrettyLocalTime,
  quote,
  getFileState,
  getSafeSetSGRCode,
  getColorize,
  getColorizeRG,
  loadRenderLines,
  loadDrugDefinitions,
  moreThanNSecondsAgo,
  loadDrugData,
  getDrugNameFromInputFilter,
  getDrugNameFromInput,
  parseEntriesCSV,
) where

import ClassyPrelude
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (TimeZone, diffUTCTime, getCurrentTimeZone, secondsToNominalDiffTime, utcToLocalTime)
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as V
import LoadConfig
import Path qualified as P
import Path.IO (doesFileExist)
import System.Console.ANSI
import System.Exit (exitFailure)
import System.Process.Typed qualified as S
import TemplateLib
import Text.Time.Pretty (prettyTimeAuto)
import Time
import Types

loadRenderLines :: Bool -> IO (Vector RenderLine)
loadRenderLines detailed = do
  drugData <- loadDrugData
  (drugDefs, _) <- loadDrugDefinitions
  now <- getCurrentTime
  lTZ <- getCurrentTimeZone

  let sortedEntries = sortOn (Down . getDate) drugData
      defMap = V.foldr insertData M.empty drugDefs
        where
          insertData DrugDefinition{..} =
            M.insert getName (getPeriod, getReminding)
      combinedEntries = V.imapMaybe (makeCombinedEntry defMap sortedEntries) sortedEntries

  if V.null combinedEntries
    then putStrLn "No entries matching existing definitions found!" >> exitFailure
    else pure . V.imap (constructRenderLine lTZ now) $ reverse combinedEntries
  where
    dateStampRel now date =
      let f = if detailed then dayhourTimeFormat now else prettyTimeAuto now
       in (<> ",") . T.pack $ f date

    makeCombinedEntry defMap sortedE i dl = do
      (period, reminding) <- M.lookup (getEntryName dl) defMap
      let isDupe = V.any ((== getEntryName dl) . getEntryName) (V.take i sortedE)
      pure (dl, isDupe, period, reminding)

    constructRenderLine localTZ now i (drugLine, old, period, reminding) =
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
            (i + 1)
            reminding

getDrugNameFromInput :: IO Text
getDrugNameFromInput = getDrugNameFromInputFilter (const True)

getDrugNameFromInputFilter :: (DrugDefinition -> Bool) -> IO Text
getDrugNameFromInputFilter f = do
  (defs, _) <- loadDrugDefinitions
  result <- runPicker . intercalate "\n" . sort $ getName <$> filter f defs
  maybe (putStrLn "Invalid input" >> exitFailure) pure result
  where
    runPicker input = do
      (exitCode, output, _) <- S.readProcess . pipeIn input $ S.proc pickerBin []
      case exitCode of
        S.ExitSuccess -> decode output
        _ -> pure Nothing
      where
        pickerBin = picker config
        pipeIn = S.setStdin . S.byteStringInput . BL8.fromStrict . T.encodeUtf8
        decode = pure . Just . TL.toStrict . TL.strip . TL.decodeUtf8

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

quote :: Text -> Text
quote t = "\"" <> t <> "\""

loadDrugDefinitions :: IO (Vector DrugDefinition, P.Path P.Abs P.File)
loadDrugDefinitions = do
  csvFile <- getCsvDrugDefinitions
  getFileState csvFile >>= \case
    FileNotExists -> haveYouRanErr
    FileEmpty -> haveYouRanErr
    FileHasContent -> do
      fileData <- BL8.fromStrict <$> B8.readFile (P.fromAbsFile csvFile)
      case parseCSV fileData of
        Left t -> terror t
        Right v -> pure (V.nubBy (comparing getName) v, csvFile)
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
