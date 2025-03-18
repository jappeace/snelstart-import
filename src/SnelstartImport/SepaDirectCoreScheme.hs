{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://www.europeanpaymentscouncil.eu/sites/default/files/kb/file/2022-06/EPC130-08%20SDD%20Core%20C2PSP%20IG%202023%20V1.0.pdf
--   this is some xml format the accountents asked support for
module SnelstartImport.SepaDirectCoreScheme
  ( SepaDirectCoreScheme(..)
  , SepaDirectCoreResults(..)
  , SepaGlobals(..)
  , readSepaDirectCoreScheme
  )
where

import SnelstartImport.Currency
import Text.XML.Hexml(parse, children, name, Node, inner)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as Text
import Data.List
import Data.Bifunctor(first)
import Text.Read(readMaybe)
import Data.Time(UTCTime, Day, parseTimeM, defaultTimeLocale)
import Data.Time.Format.ISO8601
import Data.Time.LocalTime(zonedTimeToUTC)

data SepaDirectCoreScheme = SepaDirectCoreScheme {
  -- -- | Unambiguous identification of the account of the
  -- -- creditor to which a credit entry will be posted as a
  -- -- result of the payment transaction.
  -- cdtrAcct :: Text,
  endToEndId :: Text,
  dbtrAcct :: Text, -- | bank number
  dbtr :: Text, -- | name of person sending
  instdAmt :: Currency,
  dtOfSgntr :: Day, -- | this is not the actual transaction date
  rmtInf :: Text -- | invoice number
  } deriving Show

data SepaGlobals = SepaGlobals {
  creDtTm :: UTCTime,
  cdtrAcct :: Text
  }

data SepaDirectCoreResults = SepaDirectCoreResults {
  sdcrRows :: [SepaDirectCoreScheme],
  sdcrGlob :: SepaGlobals
  }

name_ :: Node -> Text
name_ = Text.toLower . decodeUtf8 . name

dig :: Text -> Node -> [Node]
dig tag parent = filter (\x -> name_ x == Text.toLower tag) $ children parent

data SepaParseErrors = ParseXmlError ByteString
                     | SepaParseIssues SepaIssues
                     deriving Show

readSepaDirectCoreScheme :: ByteString -> Either SepaParseErrors SepaDirectCoreResults
readSepaDirectCoreScheme contents = do
  nodeRes <- first ParseXmlError $ parse contents

  mainNode :: Node <- first SepaParseIssues $ assertOne "CstmrDrctDbtInitn" ((dig "CstmrDrctDbtInitn") =<< dig "document" nodeRes)

  sdcrGlob <- first SepaParseIssues $ parseGlobals mainNode

  sdcrRows <- traverse (first SepaParseIssues . parseSepa) $ ((dig "DrctDbtTxInf")) =<< (dig "PmtInf" mainNode )

  pure $ SepaDirectCoreResults {..}
--

data SepaIssues = ExpectedOne [Node] Text
                | ExpectedNumber Node Text
                | ExpectedDate Node Text
                | ExpectedTime Node Text
                deriving Show

parseGlobals :: Node -> Either SepaIssues SepaGlobals
parseGlobals node = do
  creDtTm <- parseTime =<< assertOne "CreDtTm" (dig "CreDtTm" =<< dig "GrpHdr" node)
  cdtrAcct <- inner_ <$>  assertOne "CdtrAcct" (dig "IBAN" =<< dig "Id" =<< dig "CdtrAcct" =<< dig "PmtInf" node)
  pure $ SepaGlobals {..}

assertOne :: Text -> [Node] -> Either SepaIssues Node
assertOne label nodes =
  case uncons nodes of
    Just (x, _) -> Right x
    Nothing -> Left (ExpectedOne nodes label)

inner_ :: Node -> Text
inner_ = decodeUtf8 . inner

parseCurrency :: Node -> Either SepaIssues Currency
parseCurrency node = case readMaybe (Text.unpack (inner_ node)) of
  Just number -> Right $ Currency number
  Nothing -> Left (ExpectedNumber node (inner_ node))

parseDay :: Node -> Either SepaIssues Day
parseDay node = case parseTimeM True defaultTimeLocale "%F" (Text.unpack (inner_ node)) of
  Nothing -> Left $ ExpectedDate node (inner_ node)
  Just day -> Right day

parseTime :: Node -> Either SepaIssues UTCTime
parseTime node = case zonedTimeToUTC <$> iso8601ParseM (Text.unpack (inner_ node)) of
  Nothing -> Left $ ExpectedTime node (inner_ node)
  Just day -> Right day

parseSepa :: Node -> Either SepaIssues SepaDirectCoreScheme
parseSepa node = do
  dbtr <- inner_ <$> assertOne "dbtr" (dig "nm" =<< dig "dbtr" node)
  dbtrAcct <- inner_ <$> assertOne "dbtracct" (dig "IBAN" =<< dig "Id" =<< dig "DbtrAcct" node)
  endToEndId <- inner_ <$> assertOne "endToEndId" (dig "EndToEndId" =<< dig "PmtId" node)
  instdAmt <- parseCurrency =<< assertOne "instdAmt" (dig "instdAmt" node)
  dtOfSgntr <- parseDay =<< assertOne "dtOfSgntr" (dig "DtOfSgntr" =<< dig "MndtRltdInf" =<< dig "DrctDbtTx" node)
  rmtInf <- inner_ <$> assertOne "RmtInf" (dig "Ustrd" =<< dig "RmtInf" node)
  Right $ SepaDirectCoreScheme {
    ..
    }
