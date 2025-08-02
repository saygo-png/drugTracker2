module Remind where

import ClassyPrelude
import ConfigAndTypes
import Lib
import System.Exit (exitFailure, exitSuccess)
import System.Process.Typed (proc, runProcess_)

remind :: IO ()
remind = do
  renderLines <- loadRenderLines
  mapM_ outputRemindInfo $ filter (not . getIsOld) renderLines
  bool exitSuccess exitFailure $ any getIsMissed renderLines
  where
    outputRemindInfo :: RenderLine -> IO ()
    outputRemindInfo (RenderLine dl _ isMissed secs) = do
      let mNot = bool "" "not " isMissed

          name = getEntryName dl <> ":"
          takenOrNot = mNot <> "taken in the last"
          msg = unwords [name, takenOrNot, tshow secs, "seconds"]
          sendNotification t = runProcess_ . proc "notify-send" $ [unpack t]

      colorizeRG <- getColorizeRG
      let colored = colorizeRG (not isMissed) msg
      putStrLn colored
      when isMissed $ sendNotification msg
