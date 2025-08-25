module Json (outputJson) where

import ClassyPrelude
import Data.Aeson qualified as J
import Data.ByteString.Lazy.Char8 qualified as BL8
import Lib

outputJson :: Bool -> IO ()
outputJson = BL8.putStrLn <=< getJsonRenderLines

getJsonRenderLines :: Bool -> IO BL8.ByteString
getJsonRenderLines = (J.encode <$>) . loadRenderLines
