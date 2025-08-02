module Time (dayhourTimeFormat) where

import ClassyPrelude
import Data.Time (diffUTCTime)
import Text.Time.Pretty.TimeAgo

dayhourTimeFormat :: UTCTime -> IO String
dayhourTimeFormat before = do
  now <- getCurrentTime
  pure . customRenderTimeAgo . timeAgo $ diffUTCTime now before

customRenderTimeAgo :: TimeAgo -> String
customRenderTimeAgo ta =
  case timeAgoSign ta of
    GT -> customRender True ta -- x ago
    EQ -> "just now"
    LT -> customRender False ta -- In x

customRender :: Bool -> TimeAgo -> String
customRender agoOrIn TimeAgo{..}
  | timeAgoDays > 0 = showDaysAndHours timeAgoDays timeAgoHours
  | timeAgoHours > 0 = unwordNoEmpty [prefix, plural timeAgoHours "hour", suffix]
  | timeAgoMinutes > 0 = unwordNoEmpty [prefix, plural timeAgoMinutes "minute", suffix]
  | timeAgoSeconds > 0 = unwordNoEmpty [prefix, plural timeAgoSeconds "second", suffix]
  | otherwise = "just now"
  where
    suffix = if agoOrIn then "ago" else ""
    prefix = if agoOrIn then "" else "in"

    timeAgoDays = daysAgoToDays timeAgoDaysAgo

    unwordNoEmpty = unwords . filter (/= "")

    showDaysAndHours days hours =
      unwordNoEmpty $
        [prefix, plural days "day", suffix]
          <> if hours /= 0
            then
              ["and"] <> [plural hours "hour", suffix]
            else [prefix]

plural :: Integer -> String -> String
plural 1 sing = "1 " <> sing
plural n sing = show n <> " " <> sing <> "s"
