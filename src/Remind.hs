module Remind where

import ClassyPrelude
import Config
import Data.Fixed (Pico)
import Data.Time (diffUTCTime, secondsToNominalDiffTime)
import Lib
import System.Exit (exitFailure, exitSuccess)
import Types

remind :: IO ()
remind = do
  drugData <- fromMaybe (terror "Drug data is empty") . fromNullable . either terror id <$> loadDrugData
  now <- getCurrentTime
  let lastDate = dateData $ last drugData
  if moreThanNSecondsAgo remindPeriod lastDate now
    then putStrLn "yes, remind" >> exitSuccess
    else putStrLn "no, don't remind" >> exitFailure

moreThanNSecondsAgo :: Pico -> UTCTime -> UTCTime -> Bool
moreThanNSecondsAgo secs oldTime now =
  let diff = diffUTCTime now oldTime
      secs' = secondsToNominalDiffTime secs
   in diff > secs'
