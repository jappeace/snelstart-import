{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | deal with cli options
module SnelstartImport.Options
  ( parseProgram,
    CliOptions(..),
    ProgramOptions (..)
  )
where

import Data.Text
import           Options.Applicative

data CliOptions = CliOptions {
    cliOwnAccount :: Text
  , cliInputFile :: FilePath
  , cliOutputFile :: FilePath
  }

data ProgramOptions = Convert CliOptions
                    | Webserver

parseCli :: Parser CliOptions
parseCli = do
  cliInputFile <- option str (short 'i' <> long "input-file" <> metavar "FILE" <> help "The input file" <> value "input.csv" <> showDefault)
  cliOutputFile <- option str (short 'o' <> long "output-file" <> metavar "FILE" <> help "The output file" <> value "out.csv" <> showDefault)
  -- default is set for my own use
  cliOwnAccount <- option str (short 'a' <> long "own-account" <> metavar "ACCOUNT" <> help "The account written into" <> value "DE92100110012623092722" <> showDefault)
  pure $ CliOptions {..}

parseProgram :: Parser ProgramOptions
parseProgram =
  subparser $
    command "convert" (info (Convert <$> parseCli <**> helper) $ progDesc "Convert from cli to snelstart format")
    <>
    command "server" (info (pure Webserver) $ progDesc "Start a server to give a UI to do conversion")
