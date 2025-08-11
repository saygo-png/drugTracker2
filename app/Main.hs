module Main where

import ClassyPrelude
import ConfigAndTypes
import Create
import List
import Options.Applicative
import Remind
import Take

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOptions) (progDesc "Reminds you to take a drug")

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "take" (info (helper <*> pure CmdTake) (progDesc "Take drug."))
        <> command "list" (info (helper <*> parseList) (progDesc "List previously taken drugs."))
        <> command "create" (info (helper <*> parseCreate) (progDesc "Create a drug item."))
        <> command
          "remind"
          ( info
              (helper <*> pure CmdRemind)
              ( progDesc
                  "Check last taken time, remind if enough time elapsed since. \
                  \ Returns 0 for don't remind. Returns 1 for yes, remind."
              )
          )
    )

parseCreate :: Parser Command
parseCreate =
  CmdCreate
    <$> ( DrugDefinition
            <$> argument str (metavar "DRUG_NAME" <> help "Name of the drug to take")
            <*> argument auto (metavar "FREQUENCY" <> help "How often to take the drug in seconds")
        )

parseList :: Parser Command
parseList =
  CmdList
    <$> ( ListArgs
            <$> option
              auto
              ( long "lines"
                  <> short 'n'
                  <> metavar "NUM or \"all\""
                  <> help "How many lines from the end to show"
                  <> value (LinesInt 14)
              )
            <*> switch
              ( long "detailed"
                  <> short 'd'
                  <> help "EXPERIMENTAL: Display time with day and hour level of detail"
              )
            <*> switch
              ( long "unique"
                  <> short 'u'
                  <> help "Display the most recent drug entry for each definition. Overrides \"--lines\""
              )
        )

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

main :: IO ()
main = do
  options <- execParser parserInfo
  case optCommand options of
    CmdTake -> takeDrug
    CmdList l -> listDrugs l
    CmdRemind -> remind
    CmdCreate d -> createDrugItem d
