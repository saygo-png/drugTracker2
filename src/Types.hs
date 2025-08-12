{-# OPTIONS_GHC -Wno-orphans #-}

module Types (
  -- Csv {{{
  getEntryName,
  getDate,
  csvDefinitionsHT,
  definitionsToHeader,
  csvEntriesHT,
  entriesToHeader,
  DrugLine (DrugLine),
  RenderLine (RenderLine, getIsMissed, getIsOld, getPeriod', getDrugLine),
  DefinitionsHeader,
  EntriesHeader,
  -- }}}

  FileState (FileNotExists, FileEmpty, FileHasContent),
  DrugDefinition (DrugDefinition, getPeriod, getName),
  LinesArg (LinesInt, LinesAll),
  Command (CmdList, CmdTake, CmdRemind, CmdCreate),
  ListArgs (ListArgs, getLines, getDetailed, getUniques),
  Types.Options (Options, optCommand),
) where

import ClassyPrelude
import Control.Arrow ((>>>))
import Data.ByteString.Char8 qualified as BS8
import Data.Csv
import Data.Csv qualified as C
import Data.Fixed (Pico)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Vector qualified as Vector
import Text.ParserCombinators.ReadPrec qualified as R
import Text.Read (readPrec)
import Text.Read.Lex (Lexeme (..), lex)

csvDefinitionsHT :: DefinitionsHeader
csvDefinitionsHT = DefinitionsHeader ("name", "frequency")

csvEntriesHT :: EntriesHeader
csvEntriesHT = EntriesHeader ("drug", "date")

definitionsToHeader :: DefinitionsHeader -> C.Header
definitionsToHeader (DefinitionsHeader (a, b)) =
  Vector.fromList [a, b]

entriesToHeader :: EntriesHeader -> C.Header
entriesToHeader (EntriesHeader (a, b)) =
  Vector.fromList [a, b]

-- Types {{{

type IsMissed = Bool

type IsOld = Bool

data RenderLine = RenderLine
  { getDrugLine :: DrugLine
  , getIsOld :: IsOld
  , getIsMissed :: IsMissed
  , getPeriod' :: Integer
  }
  deriving (Show)

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

data ListArgs = ListArgs
  { getLines :: LinesArg
  , getDetailed :: Bool
  , getUniques :: Bool
  }

data Command
  = CmdList ListArgs
  | CmdTake
  | CmdRemind
  | CmdCreate DrugDefinition

newtype Options = Options {optCommand :: Command}

-- IO

data FileState = FileNotExists | FileEmpty | FileHasContent
  deriving (Eq)

-- CSV
newtype DefinitionsHeader = DefinitionsHeader (ByteString, ByteString)

newtype EntriesHeader = EntriesHeader (ByteString, ByteString)

-- This represents a line of the drugDefinitions.csv file
data DrugDefinition = DrugDefinition
  { getName :: Text
  , getPeriod :: Integer
  }
  deriving (Eq, Show)

instance ToNamedRecord DrugDefinition where
  toNamedRecord (DrugDefinition a b) =
    let
      DefinitionsHeader (col1Name, col2Frequency) = csvDefinitionsHT
     in
      namedRecord
        [col1Name .= a, col2Frequency .= b]

instance FromNamedRecord DrugDefinition where
  parseNamedRecord r =
    let
      DefinitionsHeader (col1Name, col2Frequency) = csvDefinitionsHT
     in
      DrugDefinition
        <$> r
          .: col1Name
        <*> r
          .: col2Frequency

instance ToField Pico where
  toField = tshow >>> encodeUtf8

-- This represents a line of the data.csv file {{{
data DrugLine = DrugLine
  { getEntryName :: Text
  , getDate :: UTCTime
  }
  deriving (Eq, Show)

instance FromNamedRecord DrugLine where
  parseNamedRecord r =
    let
      EntriesHeader (col1Name, col2Date) = csvEntriesHT
     in
      DrugLine
        <$> r
          .: col1Name
        <*> r
          .: col2Date

instance ToRecord DrugLine where
  toRecord (DrugLine getEntryName getDate) =
    record [toField getEntryName, toField getDate]

instance ToNamedRecord DrugLine where
  toNamedRecord (DrugLine getEntryName getDate) =
    let
      EntriesHeader (a, b) = csvEntriesHT
     in
      namedRecord
        [a .= getEntryName, b .= getDate]

instance ToField UTCTime where
  toField = iso8601Show >>> BS8.pack

instance FromField UTCTime where
  parseField = BS8.unpack >>> iso8601ParseM

-- }}}
