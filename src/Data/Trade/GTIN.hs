{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Trade.GTIN (GTIN (GTIN), GTIN14, GTIN13, GTIN12, GTIN8, EANUCC14, SCC14, EAN, EANUCC13, ISBN, ISBN13, EANUCC8, GSIN, SSCC, equivGTIN, fixChecksum, gtinToString) where

import Data.Binary (Binary (get, put))
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.List (intercalate, unfoldr)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
#if MIN_VERSION_base(4,14,3)
import Numeric.Natural (Natural)
#else
import GHC.Types(Nat)
#endif
import GHC.TypeNats (KnownNat, natVal)
import Text.Printf (printf)

#if !MIN_VERSION_base(4,14,3)
type Natural = Nat
#endif

#if MIN_VERSION_base(4,14,3)
newtype GTIN (n :: Natural) = GTIN Word64 deriving (Data, Eq, Generic, Ord, Read, Typeable)
#else
newtype GTIN (n :: Nat) = GTIN Word64 deriving (Data, Eq, Generic, Ord, Read, Typeable)
#endif

_decw :: KnownNat n => GTIN n -> Int
_decw = fromIntegral . natVal

_tocheck :: Integral i => i -> i -> i
_tocheck n d = (d + n1 + 3 * n2) `mod` 10
  where
    ~(n1, n2) = n `quotRem` 10

_determineChecksum :: Word64 -> Word64
_determineChecksum w = (10 - go (w `div` 10) 0) `mod` 10
  where
    go 0 = id
    go n = go q . _tocheck r
      where
        ~(q, r) = n `quotRem` 100

fixChecksum :: GTIN n -> GTIN n
fixChecksum (GTIN w') = GTIN (w' - w' `mod` 10 + _determineChecksum w')

equivGTIN :: GTIN m -> GTIN n -> Bool
equivGTIN (GTIN w1) (GTIN w2) = w1 == w2

instance KnownNat n => Show (GTIN n) where
  showsPrec d g@(GTIN v) = showParen (d > 0) (("GTIN " ++ printf ("%0" ++ sn ++ "d") v ++ " :: GTIN " ++ sn) ++)
    where
      sn = show (_decw g)

gtinToString :: KnownNat n => GTIN n -> String
gtinToString g@(GTIN w) = intercalate " " (map p (reverse (unfoldr f (n, w))))
  where
    n = _decw g
    p (n0, v) = printf ("%0" ++ show n0 ++ "d") v
    f (n0, v)
      | n0 <= 0 = Nothing
      | otherwise = Just ((min 4 n0, r), (n0 - dd, q))
      where
        ~(q, r) = v `quotRem` 10000
        dd = min 4 n0

instance Hashable (GTIN n)

instance Binary (GTIN n) where
  get = GTIN <$> get
  put (GTIN w) = put w

instance KnownNat n => Bounded (GTIN (n :: Natural)) where
  minBound = GTIN 0
  maxBound = GTIN (10 ^ _decw (error "should not be evaluated" :: GTIN n) - 1)

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
