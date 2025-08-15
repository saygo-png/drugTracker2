module Remind where

import ClassyPrelude
import Lib
import System.Exit (exitFailure, exitSuccess)
import System.Process.Typed (proc, runProcess_)
import Types

remind :: IO ()
remind = do
  renderLines <- loadRenderLines False
  let forReminding = filter (liftA2 (&&) (not . getIsOld) getReminding') renderLines
  mapM_ outputRemindInfo forReminding
  when (null forReminding) $ putStrLn "No definitions to show!"
  bool exitSuccess exitFailure $ any (liftA2 (&&) getIsMissed getReminding') renderLines
  where
    outputRemindInfo :: RenderLine -> IO ()
    outputRemindInfo RenderLine{..} = do
      let mNot = bool "" "not " getIsMissed

          name = getEntryName getDrugLine <> ":"
          takenOrNot = mNot <> "taken in the last"
          msg = unwords [name, takenOrNot, tshow getPeriod', "seconds"]
          sendNotification t = runProcess_ . proc "notify-send" $ [unpack t]

      colorizeRG <- getColorizeRG
      let colored = colorizeRG (not getIsMissed) msg
      putStrLn colored
      when getIsMissed $ sendNotification msg
