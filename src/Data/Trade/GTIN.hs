module Data.Trade.GTIN (GTIN (GTIN)) where

import Data.Word(Word64)

data GTIN = GTIN Word64 deriving (Eq, Ord, Read, Show)
