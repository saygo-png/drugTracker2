module Remind (remind) where

import ClassyPrelude
import Lib
import System.Exit (exitFailure, exitSuccess)
import System.Process.Typed (proc, runProcess_)
import Types

remind :: IO ()
remind = do
  renderLines <- loadRenderLines False
  let forReminding = filter (liftA2 (&&) (not . (.isOld)) (.reminding)) renderLines
  mapM_ outputRemindInfo forReminding
  when (null forReminding) $ putStrLn "No definitions to show!"
  bool exitSuccess exitFailure $ any (liftA2 (&&) (.isMissed) (.reminding)) renderLines
  where
    outputRemindInfo :: RenderLine -> IO ()
    outputRemindInfo rl = do
      let mNot = bool "" "not " rl.isMissed

          name = rl.drugLine.name <> ":"
          takenOrNot = mNot <> "taken in the last"
          msg = unwords [name, takenOrNot, tshow rl.period, "seconds"]
          sendNotification t = runProcess_ . proc "notify-send" $ [unpack t]

      colorizeRG <- getColorizeRG
      let colored = colorizeRG (not rl.isMissed) msg
      putStrLn colored
      when rl.isMissed $ sendNotification msg
