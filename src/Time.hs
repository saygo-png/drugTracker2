module Time (dayhourTimeFormat) where

import ClassyPrelude
import Data.Time (diffUTCTime)
import Text.Time.Pretty.TimeAgo

dayhourTimeFormat :: UTCTime -> UTCTime -> String
dayhourTimeFormat now before = customRenderTimeAgo . timeAgo . flip diffUTCTime before $ now

customRenderTimeAgo :: TimeAgo -> String
customRenderTimeAgo ta =
  case ta.timeAgoSign of
    GT -> customRender True ta -- x ago
    EQ -> "just now"
    LT -> customRender False ta -- In x

customRender :: Bool -> TimeAgo -> String
customRender agoOrIn ta
  | timeAgoDays > 0 = showDaysAndHours timeAgoDays ta.timeAgoHours
  | ta.timeAgoHours > 0 = unwordNoEmpty [prefix, plural ta.timeAgoHours "hour", suffix]
  | ta.timeAgoMinutes > 0 = unwordNoEmpty [prefix, plural ta.timeAgoMinutes "minute", suffix]
  | ta.timeAgoSeconds > 0 = unwordNoEmpty [prefix, plural ta.timeAgoSeconds "second", suffix]
  | otherwise = "just now"
  where
    suffix = if agoOrIn then "ago" else ""
    prefix = if agoOrIn then "" else "in"

    timeAgoDays = daysAgoToDays ta.timeAgoDaysAgo

    unwordNoEmpty = unwords . filter (/= "")

    showDaysAndHours days hours =
      unwordNoEmpty $
        [prefix, plural days "day", suffix]
          <> if hours /= 0
            then
              ["and"] <> [plural hours "hour", suffix]
            else [prefix]

    plural 1 sing = "1 " <> sing
    plural n sing = show n <> " " <> sing <> "s"
