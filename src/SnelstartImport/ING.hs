{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

-- | We target the ING bank as the output format
module SnelstartImport.ING
  ( ING(..),
    toCode,
    writeCsv,
    MutatieSoort (..),
    BijAf(..)
  )
where

import SnelstartImport.Currency
import Data.Text.Encoding (encodeUtf8)
import Data.Text
import Data.Text.Encoding
import Data.Csv
import Data.Time
import GHC.Generics
import qualified Data.Vector as Vector
import NeatInterpolation(text)
import qualified Data.ByteString.Lazy as LBS

data MutatieSoort = Overschijving | Diversen | Incasso
  deriving Show

mutatieSoortToFIeld :: MutatieSoort -> Text
mutatieSoortToFIeld = \case
  Overschijving -> "Overschrijving"
  Diversen -> "Diversen"
  Incasso -> "Incasso"

toCode :: MutatieSoort -> Text
toCode = \case
  Overschijving -> "OV"
  Diversen -> "DV"
  Incasso -> "IC"

data BijAf = Bij | Af
  deriving Show

bijAfToField :: BijAf -> Text
bijAfToField = \case
  Bij -> "Bij"
  Af -> "Af"


data ING = ING {
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

instance ToRecord ING where
  toRecord (ING{..}) =
    Vector.fromList
      [ toField $ formatTime defaultTimeLocale "%Y%m%d" datum
      , toField naamBescrhijving
      , toField rekening
      , toField tegenRekening
      , toField $ toCode mutatieSoort
      , toField $ bijAfToField bijAf
      , toField $ replaceDotWithComma <$>  Prelude.show bedragEur
      , toField $ mutatieSoortToFIeld mutatieSoort
      , toField mededeling
      ]

data TransactionType = MastercardPayment | OutgoingTransfer | Income | N26Referal | DirectDebit
  deriving stock (Show, Eq)

replaceDotWithComma :: Char -> Char
replaceDotWithComma '.' = ','
replaceDotWithComma other = other

instance FromField TransactionType where
  parseField field = case decodeUtf8 field of
    "MasterCard Payment" -> pure MastercardPayment
    "Outgoing Transfer" -> pure OutgoingTransfer
    "Income" -> pure Income
    "N26 Referral" -> pure N26Referal
    "Direct Debit" -> pure DirectDebit
    other -> fail $ "TransactionType unkown" <> unpack other

newtype Date = Date UTCTime
  deriving newtype (Show, Eq, Read)

instance FromField Date where
  parseField field =
    fmap Date $ parseTimeM True defaultTimeLocale "%Y-%m-%d" $ unpack $ decodeUtf8 field


writeCsv :: [ING] -> LBS.ByteString
writeCsv lines = LBS.fromStrict header' <> data'
  where
        data' :: LBS.ByteString
        data' = encodeWith opts lines
        header' = encodeUtf8 $ [text|"Datum","Naam / Omschrijving","Rekening","Tegenrekening","Code","Af Bij","Bedrag (EUR)","Mutatiesoort","Mededelingen"|] <> "\n"

opts :: EncodeOptions
opts = defaultEncodeOptions { encQuoting = QuoteAll}
