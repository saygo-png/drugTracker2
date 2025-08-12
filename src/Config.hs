module Config (
  colText,
  rowText,
  fuzzyFinder,
) where

import ClassyPrelude

colText :: Text
colText = " | "

rowText :: Text
rowText = "-"

fuzzyFinder :: String
fuzzyFinder = "fzf"
