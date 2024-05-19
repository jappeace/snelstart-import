{-# LANGUAGE DeriveAnyClass #-}
module SnelstartN26Import
  ( main
  )
where

import Data.Text
import Data.Text.Encoding
import Data.Csv
import Data.Time
import GHC.Generics
import qualified Data.Attoparsec.Text as Atto
import Data.Ratio

data MutatieSoort = Overschijving | Diversen | Incasso
  deriving Show

toCode :: MutatieSoort -> Text
toCode = \case
  Overschijving -> "OV"
  Diversen -> "DV"
  Incasso -> "IC"

data BijAf = Bij | Af
  deriving Show

newtype Currency = Currency Rational
  deriving newtype Show

data Snelstart = Snelstart {
  datum :: UTCTime,
  naamBescrhijving :: Text,
  rekening :: Text,
  tegenRekening :: Text,
  mutatieSoort ::  MutatieSoort, -- eg ook voor code
  bijAf :: BijAf,
  bedragEur :: Currency ,
  mededeling :: Text
  }
  deriving stock (Generic, Show)

data TransactionType = MastercardPayment | OutgoingTransfer | Income | N26Referal | DirectDebit
  deriving Show


parseCurrency :: Atto.Parser Currency
parseCurrency = do
  nominals <- Atto.decimal `Atto.sepBy` Atto.char ','
  case nominals of
    one : two : [] -> pure $ Currency $ one % two
    one : [] -> pure $ Currency $ one % 1
    _ -> fail "to many or to few"

instance FromField Currency where
  parseField field = case Atto.parse parseCurrency $ decodeUtf8 field of
    Atto.Done _ r -> pure r
    Atto.Partial _ ->  fail $ "Currency.incomplete"
    Atto.Fail _ s y -> fail $ "Currency.failed because " <> show (s,y)

instance FromField TransactionType where
  parseField field = case decodeUtf8 field of
    "MasterCard Payment" -> pure MastercardPayment
    "Outgoing Transfer" -> pure OutgoingTransfer
    "Income" -> pure Income
    "N26 Referral" -> pure N26Referal
    "Direct Debit" -> pure DirectDebit
    other -> fail $ "TransactionType unkown" <> unpack other

newtype Date = Date UTCTime
  deriving newtype Show

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
  amountForegin :: Currency ,
  typeForeign :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass FromRecord


main :: IO ()
main = putStrLn "hello, world flaky"
