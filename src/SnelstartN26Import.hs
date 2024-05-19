module SnelstartN26Import
  ( main
  )
where

import Data.Text

data MutatieSoort = Overschijving | Diversen | Incassoe

toCode :: MutatieSoort
toCode = \case
  Overschijving -> "OV"
  Diversen -> "DV"
  Incasso -> "IC"

data BijAf = Bij | Af

data Snelstart = Snelstart {
  datum :: UTCTime,
  naamBescrhijving :: Text,
  rekening :: Text,
  tegenRekening :: Text,
  mutatieSoort ::  MutatieSoort, -- eg ook voor code
  bijAf :: BijAf,
  bedragEur :: Rational,
  mededeling :: Text
  }

data TransactionType = MastercardPayment | OutgoingTransfer | Income | N26Referal | DirectDebit

data N26 = N26  {
  date :: UTCTime,
  payee :: Text,
  accountNumber :: Text
  }


main :: IO ()
main = putStrLn "hello, world flaky"
