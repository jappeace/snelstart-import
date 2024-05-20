{-# LANGUAGE DeriveAnyClass #-}
module SnelstartN26Import.N26
  ( readN26,
    N26 (..),
    TransactionType(..),
    Date(..)
  )
where

import SnelstartN26Import.Currency
import Data.Text
import Data.Text.Encoding
import Data.Csv
import Data.Time
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Data.Vector

data MutatieSoort = Overschijving | Diversen | Incasso
  deriving Show

data BijAf = Bij | Af
  deriving Show

data TransactionType = MastercardPayment | OutgoingTransfer | Income | N26Referal | DirectDebit
  deriving stock (Show, Eq)


instance FromField TransactionType where
  parseField field = case decodeUtf8 field of
    "MasterCard Payment" -> pure MastercardPayment
    "Outgoing Transfer" -> pure OutgoingTransfer
    "Income" -> pure Income
    "N26 Referral" -> pure N26Referal
    "Direct Debit" -> pure DirectDebit
    other -> fail $ "TransactionType unkown" <> unpack other

newtype Date = Date { unDate :: UTCTime }
  deriving newtype (Show, Eq, Read)

instance FromField Date where
  parseField field =
    fmap Date $ parseTimeM True defaultTimeLocale "%Y-%m-%d" $ unpack $ decodeUtf8 field

data N26 = N26  {
  date :: Date,
  payee :: Text,
  accountNumber :: Text,
  transactionType :: TransactionType,
  paymentReference :: Text,
  amountEur :: Currency ,
  amountForegin :: Maybe Currency ,
  typeForeign :: Text,
  exchangeRate :: Text -- unused
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass FromRecord


readN26 :: FilePath -> IO (Either String (Vector N26))
readN26 path = do
  lines' <- BS.readFile path
  pure $ decode HasHeader lines'
