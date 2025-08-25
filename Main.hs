module Main (main) where

import ClassyPrelude
import Create
import EnableDisable
import List
import Options.Applicative
import Remind
import Status
import Take
import Types

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOptions) (progDesc "Reminds you to take a drug")

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "take" (info (helper <*> pure CmdTake) (progDesc "Take drug."))
        <> command "list" (info (helper <*> parseList) (progDesc "List previously taken drugs."))
        <> command "create" (info (helper <*> parseCreate) (progDesc "Create a drug item."))
        <> command "disable" (info (helper <*> pure CmdDisable) (progDesc "Stop reminding about drug item."))
        <> command "status" (info (helper <*> pure CmdStatus) (progDesc "Show status of drug entries."))
        <> command "enable" (info (helper <*> pure CmdEnable) (progDesc "Start reminding about drug item."))
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
            <*> pure True
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
  case options.optCommand of
    CmdTake -> takeDrug
    CmdList l -> listDrugs l
    CmdRemind -> remind
    CmdEnable -> enable
    CmdDisable -> disable
    CmdStatus -> status
    CmdCreate d -> createDrugItem d
