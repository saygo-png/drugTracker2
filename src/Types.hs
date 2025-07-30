{-# LANGUAGE GHC2021 #-}

module Types (
  FileState (FileNotExists, FileEmpty, FileHasContent),
  DrugLine (DrugLine),
  drugData,
  dateData,
  LinesArg (LinesInt, LinesAll),
  Command (CmdList, CmdTake),
  Types.Options (Options, optCommand),
) where

import ClassyPrelude
import Data.ByteString.Char8 qualified as BS8
import Data.Csv
import Data.Function ((&))
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Text.ParserCombinators.ReadPrec qualified as R
import Text.Read (readPrec)
import Text.Read.Lex (Lexeme (..), lex)

data LinesArg = LinesInt Int | LinesAll
  deriving (Eq, Show)

instance Read LinesArg where
  readPrec =
    (LinesInt <$> readPrec) R.<++ do
      lexeme <- R.lift lex
      case lexeme of
        Ident "all" -> return LinesAll
        Ident _ -> R.pfail -- Any other string fails
        _ -> R.pfail

data Command = CmdList LinesArg | CmdTake Text

newtype Options = Options
  { optCommand :: Command
  }

data FileState = FileNotExists | FileEmpty | FileHasContent

data DrugLine = DrugLine
  { drugData :: Maybe Text
  , dateData :: UTCTime
  }
  deriving (Eq, Show)

instance FromNamedRecord DrugLine where
  parseNamedRecord m =
    DrugLine
      <$> m
        .: "drug"
      <*> m
        .: "date"

instance ToRecord DrugLine where
  toRecord (DrugLine drugData dateData) =
    record [toField drugData, toField dateData]

instance ToNamedRecord DrugLine where
  toNamedRecord (DrugLine drugData dateData) =
    namedRecord
      ["drug" .= drugData, "date" .= dateData]

instance ToField UTCTime where
  toField i = iso8601Show i & BS8.pack

instance FromField UTCTime where
  parseField i = BS8.unpack i & iso8601ParseM
