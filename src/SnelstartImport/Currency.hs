{-# LANGUAGE DeriveAnyClass #-}
module SnelstartImport.Currency
  ( Currency(..)
  )
where

import Data.Csv

newtype Currency = Currency Double
  deriving newtype (Ord, Show, Eq, FromField, Num, Fractional)
