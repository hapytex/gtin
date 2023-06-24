{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Trade.GTIN (GTIN (GTIN), GTIN14, GTIN13, GTIN12, GTIN8, EANUCC14, SCC14, EAN, EANUCC13, ISBN, ISBN13, EANUCC8) where

import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, natVal)
import Numeric.Natural (Natural)
import Text.Printf(printf)

newtype GTIN (n :: Natural) = GTIN Word64 deriving (Data, Eq, Generic, Ord, Read, Typeable)

_decw :: KnownNat n => GTIN n -> Natural
_decw = natVal

_tocheck :: Integral i => i -> i
_tocheck n = n1 + 3 * n2
  where ~(n1, n2) = n `quotRem` 10

instance KnownNat n => Show (GTIN n) where
  showsPrec d g@(GTIN v) = showParen (d > 0) (("GTIN " ++ printf ("%0" ++ sn ++ "d") v ++ " :: GTIN " ++ sn) ++)
    where sn = show (_decw g)

instance Hashable (GTIN n)

instance KnownNat n => Bounded (GTIN (n :: Natural)) where
  minBound = GTIN 0
  maxBound = GTIN (10 ^ _decw (undefined :: GTIN n) - 1)

type GTIN14 = GTIN 14

type GTIN13 = GTIN 13

type GTIN12 = GTIN 12

type GTIN8 = GTIN 8

type GSIN = GTIN 17

type SSCC = GTIN 18

type EANUCC14 = GTIN14

type SCC14 = GTIN14

type EAN = GTIN13

type EANUCC13 = GTIN13

type ISBN = GTIN13

type ISBN13 = GTIN13

type EANUCC8 = GTIN8
