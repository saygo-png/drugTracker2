module Lib (
  toPrettyLocalTime,
) where

import ClassyPrelude
import Data.Function ((&))
import Data.Time (getCurrentTimeZone, utcToLocalTime)

toPrettyLocalTime :: UTCTime -> IO Text
toPrettyLocalTime utcTime = do
  localTZ <- getCurrentTimeZone
  utcToLocalTime localTZ utcTime & tshow & takeWhile (/= '.') & pure
