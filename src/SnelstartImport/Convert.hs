{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- | Go from various formats to snelstart (ing)
module SnelstartImport.Convert
  ( n26ToING,
    sepaDirectCoreSchemeToING
  )
where

import SnelstartImport.ING
import SnelstartImport.N26
import Data.Text(Text)
import SnelstartImport.SepaDirectCoreScheme

sepaDirectCoreSchemeToING :: SepaGlobals -> SepaDirectCoreScheme -> ING
sepaDirectCoreSchemeToING SepaGlobals{..} SepaDirectCoreScheme{..} = ING{
  datum = creDtTm ,
  naamBescrhijving = dbtr,
  rekening = cdtrAcct,
  tegenRekening  = dbtrAcct,
  mutatieSoort = Overschijving, -- TODO how can we figure this out?
  bijAf = Bij, -- appaerantly they ony use it for invoices so they add money
  bedragEur = instdAmt ,
  mededeling = rmtInf
  }

n26ToING :: Text -> N26 -> ING
n26ToING ownAccoun N26{..} = ING {
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
