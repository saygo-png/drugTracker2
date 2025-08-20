{-# OPTIONS_GHC -Wno-orphans #-}

module Types (
  -- Csv {{{
  csvDefinitionsHT,
  definitionsToHeader,
  csvEntriesHT,
  entriesToHeader,
  DrugLine (DrugLine, date, name),
  RenderLine (RenderLine, reminding, isMissed, index, dateRel, dateAbs, isOld, period, drugLine),
  DefinitionsHeader,
  EntriesHeader,
  -- }}}
  IsMissed,
  IsOld,
  FileState (FileNotExists, FileEmpty, FileHasContent),
  DrugDefinition (DrugDefinition, period, name, reminding),
  LinesArg (LinesInt, LinesAll),
  Command (CmdList, CmdTake, CmdStatus, CmdRemind, CmdCreate, CmdEnable, CmdDisable),
  Config (Config, columnString, rowString, picker),
  ListArgs (ListArgs, lines, detailed, uniques),
  Types.Options (Options, optCommand),
) where

import ClassyPrelude
import Control.Arrow ((>>>))
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as BS8
import Data.Csv
import Data.Csv qualified as C
import Data.Fixed (Pico)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Vector qualified as Vector
import Language.Haskell.TH.Syntax (Lift)
import Text.ParserCombinators.ReadPrec qualified as R
import Text.Read (readPrec)
import Text.Read.Lex (Lexeme (..), lex)

csvDefinitionsHT :: DefinitionsHeader
csvDefinitionsHT = DefinitionsHeader ("name", "frequency", "reminding")

csvEntriesHT :: EntriesHeader
csvEntriesHT = EntriesHeader ("drug", "date")

definitionsToHeader :: DefinitionsHeader -> C.Header
definitionsToHeader (DefinitionsHeader (a, b, c)) =
  Vector.fromList [a, b, c]

entriesToHeader :: EntriesHeader -> C.Header
entriesToHeader (EntriesHeader (a, b)) =
  Vector.fromList [a, b]

data Config = Config
  { columnString :: Text
  , rowString :: Text
  , picker :: String
  }
  deriving (Show, Lift)

instance J.FromJSON Config where
  parseJSON = J.withObject "Config" $ \o ->
    Config
      <$> o J..: "columnString"
      <*> o J..: "rowString"
      <*> o J..: "picker"

type IsMissed = Bool

type IsOld = Bool

data RenderLine = RenderLine
  { drugLine :: DrugLine
  , isOld :: IsOld
  , isMissed :: IsMissed
  , period :: Integer
  , dateRel :: Text
  , dateAbs :: Text
  , index :: Int
  , reminding :: Bool
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
  { lines :: LinesArg
  , detailed :: Bool
  , uniques :: Bool
  }

data Command
  = CmdList ListArgs
  | CmdTake
  | CmdStatus
  | CmdRemind
  | CmdCreate DrugDefinition
  | CmdDisable
  | CmdEnable

newtype Options = Options {optCommand :: Command}

-- IO

data FileState = FileNotExists | FileEmpty | FileHasContent
  deriving (Eq)

-- CSV
newtype DefinitionsHeader = DefinitionsHeader (ByteString, ByteString, ByteString)

newtype EntriesHeader = EntriesHeader (ByteString, ByteString)

-- This represents a line of the drugDefinitions.csv file
data DrugDefinition = DrugDefinition
  { name :: Text
  , period :: Integer
  , reminding :: Bool
  }
  deriving (Eq, Show)

instance ToNamedRecord DrugDefinition where
  toNamedRecord (DrugDefinition a b c) =
    let
      DefinitionsHeader (col1Name, col2Frequency, col3Reminding) = csvDefinitionsHT
     in
      namedRecord
        [col1Name .= a, col2Frequency .= b, col3Reminding .= c]

instance FromNamedRecord DrugDefinition where
  parseNamedRecord r =
    let
      DefinitionsHeader (col1Name, col2Frequency, col3Reminding) = csvDefinitionsHT
     in
      DrugDefinition
        <$> r
          .: col1Name
        <*> r
          .: col2Frequency
        <*> r
          .: col3Reminding

instance ToField Bool where
  toField = tshow >>> encodeUtf8

instance FromField Bool where
  parseField field
    | field == "true" = pure True
    | field == "True" = pure True
    | field == "false" = pure False
    | field == "False" = pure False
    | otherwise = terror $ "Invalid boolean value: " <> decodeUtf8 field

instance ToField Pico where
  toField = tshow >>> encodeUtf8

-- This represents a line of the data.csv file {{{
data DrugLine = DrugLine
  { name :: Text
  , date :: UTCTime
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
