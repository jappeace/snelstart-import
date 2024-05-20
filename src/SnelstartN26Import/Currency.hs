{-# LANGUAGE DeriveAnyClass #-}
module SnelstartN26Import.Currency
  ( Currency(..)
  )
where

import Data.Text
import Data.Text.Encoding
import Data.Csv
import Data.Time
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Data.Vector


newtype Currency = Currency Double
  deriving newtype (Ord, Show, Eq, FromField, Num, Fractional)
