{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.Trade.GTIN (GTIN (GTIN)) where

import Data.Word (Word64)
import GHC.Num.Natural (Natural)

newtype GTIN (n :: Natural) = GTIN Word64 deriving (Eq, Ord, Read, Show)

type GTIN14 = GTIN 14

type EAN = GTIN14

type GTIN8 = GTIN 8
