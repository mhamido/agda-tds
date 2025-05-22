module Doogle.Options (
    Option (..),
    parseCliOptions,
) where

import Options.Applicative

data Option
    = -- | Directory to build the index from.
      BuildIndex String
    | -- | Run in interactive mode (from the command line, with a prelude).
      Interactive String
    | -- | Run in interactive mode, with JSON output.
      JsonInteractive
    deriving (Show)

commandParser :: Parser Option
commandParser =
    BuildIndex
        <$> strOption
            ( long "build-index"
                <> short 'b'
                <> metavar "DIR"
                <> help "Build index from the given directory"
            )
    <|> Interactive
        <$> strOption
            ( long "interactive"
                <> short 'i'
                <> help "Run in interactive mode from the given `Prelude` file."
            )
    <|> flag'
        JsonInteractive
            ( long "json-interactive"
                <> short 'j'
                <> help "Run in JSON interactive mode"
            )

parseCliOptions :: IO Option
parseCliOptions = execParser $ info (commandParser <**> helper) (fullDesc <> header "Doogle - Hoogle, but for Agda!")
