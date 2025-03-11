{-# LANGUAGE DeriveAnyClass #-}

-- | https://www.europeanpaymentscouncil.eu/sites/default/files/kb/file/2022-06/EPC130-08%20SDD%20Core%20C2PSP%20IG%202023%20V1.0.pdf
--   this is some xml format the accountents asked support for
module SnelstartImport.SepaDirectCoreScheme
  ( SepaDirectCoreScheme(..)
  )
where

import SnelstartImport.Currency

data SepaDirectCoreScheme = SepaDirectCoreScheme {
  -- | Unambiguous identification of the account of the
  -- creditor to which a credit entry will be posted as a
  -- result of the payment transaction.
  cdtrAcct :: Text,
  }
