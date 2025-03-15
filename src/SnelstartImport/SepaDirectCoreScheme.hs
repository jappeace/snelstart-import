{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://www.europeanpaymentscouncil.eu/sites/default/files/kb/file/2022-06/EPC130-08%20SDD%20Core%20C2PSP%20IG%202023%20V1.0.pdf
--   this is some xml format the accountents asked support for
module SnelstartImport.SepaDirectCoreScheme
  ( SepaDirectCoreScheme(..)
  , readSepaDirectCoreScheme
  )
where

import SnelstartImport.Currency
import Text.XML.Hexml(parse, children, name, Node, inner)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as Text
import Data.List
import Data.Bifunctor(first)
import Text.Read(readMaybe)
import Data.Time(Day, parseTimeM, defaultTimeLocale)

data SepaDirectCoreScheme = SepaDirectCoreScheme {
  -- -- | Unambiguous identification of the account of the
  -- -- creditor to which a credit entry will be posted as a
  -- -- result of the payment transaction.
  -- cdtrAcct :: Text,
  endToEndId :: Text,
  dbtrAcct :: Text,
  dbtr :: Text,
  instdAmt :: Currency,
  dtOfSgntr :: Day
  } deriving Show

name_ :: Node -> Text
name_ = Text.toLower . decodeUtf8 . name

dig :: Text -> Node -> [Node]
dig tag parent = filter (\x -> name_ x == Text.toLower tag) $ children parent

data SepaParseErrors = ParseXmlError ByteString
                     | SepaParseIssues SepaIssues
                     deriving Show

readSepaDirectCoreScheme :: ByteString -> Either SepaParseErrors [SepaDirectCoreScheme]
readSepaDirectCoreScheme contents = do
  nodeRes <- first ParseXmlError $ parse contents

  traverse (first SepaParseIssues . parseSepa) (((dig "DrctDbtTxInf")) =<< ((dig "PmtInf") =<< ((dig "CstmrDrctDbtInitn") =<< dig "document" nodeRes)))
--

data SepaIssues = ExpectedOne [Node] Text
                | ExpectedNumber Node Text
                | ExpectedDate Node Text
                deriving Show


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

parseSepa :: Node -> Either SepaIssues SepaDirectCoreScheme
parseSepa node = do
  dbtr <- inner_ <$> assertOne "dbtr" (dig "nm" =<< dig "dbtr" node)
  dbtrAcct <- inner_ <$> assertOne "dbtracct" (dig "IBAN" =<< dig "Id" =<< dig "DbtrAcct" node)
  endToEndId <- inner_ <$> assertOne "endToEndId" (dig "EndToEndId" =<< dig "PmtId" node)
  instdAmt <- parseCurrency =<< assertOne "instdAmt" (dig "instdAmt" node)
  dtOfSgntr <- parseDay =<< assertOne "dtOfSgntr" (dig "DtOfSgntr" =<< dig "MndtRltdInf" =<< dig "DrctDbtTx" node)
  Right $ SepaDirectCoreScheme {
    ..
    }
