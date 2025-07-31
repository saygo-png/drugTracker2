module Main where

import ClassyPrelude
import List
import Options.Applicative
import Take
import Types

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOptions) (progDesc "Reminds you to take a drug")

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "take" (info parseTake (progDesc "Take drug"))
        <> command "list" (info (helper <*> parseList) (progDesc "List previously taken drugs"))
    )

parseTake :: Parser Command
parseTake = CmdTake <$> argument str (metavar "DRUG_NAME" <> help "Name of the drug to take")

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
                  <> help "EXPERIMENTAL: Whether to display time with day and hour level of detail"
              )
        )

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

main :: IO ()
main = do
  options <- execParser parserInfo
  case optCommand options of
    CmdTake drugName -> takeDrug drugName
    CmdList listArgs -> listDrugs listArgs
