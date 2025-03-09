{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module SnelstartImport
  ( main,
  )
where

import NeatInterpolation
import qualified Data.ByteString.Lazy as BS
import SnelstartImport.Snelstart
import SnelstartImport.N26
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector(toList)
import Data.Csv(EncodeOptions (..), defaultEncodeOptions, Quoting (..), encodeWith)
import Data.ByteString.Lazy (ByteString)
import SnelstartImport.Options
import Paths_snelstart_import (version)
import           Options.Applicative
import Text.Printf
import Data.Version (showVersion)
import SnelstartImport.Web
import SnelstartImport.Convert

currentVersion :: String
currentVersion = showVersion version

readSettings :: IO (ProgramOptions)
readSettings = customExecParser (prefs showHelpOnError) $ info
  (helper <*> parseProgram)
  (fullDesc <> header (printf "Snelstart importer %s" currentVersion) <> progDesc
    "Converts various banks and programs to something snelstart understands"
  )


main :: IO ()
main = do
  settings <- readSettings
  case settings of
    Convert cli -> convertCli cli
    Webserver options -> webMain options

convertCli :: CliOptions -> IO ()
convertCli options = do
  result <- readN26 (cliInputFile options)
  case result of
    Left x -> error x
    Right n26Vec -> BS.writeFile (cliOutputFile options) $ let
        n26 :: [Snelstart ]
        n26 = toSnelstart (cliOwnAccount options) <$> toList n26Vec
      in
        writeCsv n26
