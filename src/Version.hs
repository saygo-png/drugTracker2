{-# LANGUAGE TemplateHaskell #-}

module Version (getGitRev) where

import ClassyPrelude
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax qualified as TH
import System.Process.Typed (proc, readProcess)

getGitRev :: Q Exp
getGitRev = do
  (_exitCode, output, _) <- runIO (readProcess $ proc "sh" ["-c", "git rev-parse HEAD"])
  TH.lift . TL.unpack $ TL.decodeUtf8 output
