{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- | Go from various formats to snelstart (ing)
module SnelstartImport.Convert
  ( toSnelstart
  )
where

import SnelstartImport.Snelstart
import SnelstartImport.N26
import Data.Text(Text)


toSnelstart :: Text -> N26 -> Snelstart
toSnelstart ownAccoun N26{..} = Snelstart {
  datum = unDate date,
  naamBescrhijving = payee,
  rekening = ownAccoun,
  tegenRekening  = accountNumber,
  mutatieSoort = toType transactionType , -- eg ook voor code
  bijAf = if amountEur < 0 then Af else Bij,
  bedragEur = abs amountEur ,
  mededeling = paymentReference
  }

toType :: TransactionType -> MutatieSoort
toType = \case
  MastercardPayment -> Diversen
  OutgoingTransfer -> Overschijving
  Income -> Overschijving
  N26Referal -> Diversen
  DirectDebit -> Overschijving
