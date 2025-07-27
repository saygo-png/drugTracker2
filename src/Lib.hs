module Lib (
  toPrettyLocalTime,
) where

import ClassyPrelude
import Data.Function ((&))
import Data.Time (getCurrentTimeZone, utcToLocalTime)

toPrettyLocalTime :: UTCTime -> IO Text
toPrettyLocalTime utcTime = do
  localTZ <- getCurrentTimeZone
  let text = utcToLocalTime localTZ utcTime & tshow
  takeWhile (/= '.') text & dropEnd 3 & pure
