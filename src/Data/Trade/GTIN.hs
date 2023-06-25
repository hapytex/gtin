{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if !MIN_VERSION_base(4,17,1)
{-# LANGUAGE TypeFamilies #-}
#endif
{-# OPTIONS_GHC -Wall -fno-warn-redundant-constraints -Werror #-}

-- |
-- Module      : Data.Trade.GTIN
-- Description : A module to parse, render and manipulate GTIN codes used for trade and EAN barcodes.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The module exposes a 'GTIN' data type that contains the number of digits as well.
module Data.Trade.GTIN
  ( -- * GTIN and its aliasses.
    GTIN (GTIN),
    gtin,
    GTIN14,
    GTIN13,
    GTIN12,
    GTIN8,
    EANUCC14,
    SCC14,
    EAN,
    EANUCC13,
    ISBN,
    ISBN13,
    EANUCC8,
    GSIN,
    SSCC,

    -- * Check if two GTINs are equivalent, even if the "width" of the GTINs are equivalent.
    equivGTIN,

    -- * Fix the checksum of a GTIN number
    fixChecksum,
    checkChecksum,

    -- * Convert the GTINs to a readable format.
    gtinToString,
  )
where

import Data.Binary (Binary (get, put))
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.List (unfoldr)
import Data.Typeable (Typeable)
#if MIN_VERSION_validity(0,9,0)
import Data.Validity (Validity (validate), check)
#else
import Data.Validity (Validation(Validation), Validity (validate), check)
#endif
import Data.Word (Word64)
import GHC.Generics (Generic)
#if MIN_VERSION_base(4,16,4)
import Numeric.Natural (Natural)
#else
import GHC.Types(Nat)
#endif
import GHC.TypeNats (KnownNat, natVal)
import qualified GHC.TypeNats as TN
import Text.Printf (printf)

#if MIN_VERSION_base(4,16,4)
-- | A datatype for /Global Trade Item Numbers 'GTIN'/ with arbitrary "width" (up to nineteen digits technically possible).
newtype GTIN (n :: Natural) = GTIN Word64 deriving (Data, Eq, Generic, Ord, Read, Typeable)
#else
-- | A datatype for /Global Trade Item Numbers 'GTIN'/ with arbitrary "width" (up to nineteen digits technically possible).
newtype GTIN (n :: Nat) = GTIN Word64 deriving (Data, Eq, Generic, Ord, Read, Typeable)
#endif

-- | Constructing a 'GTIN" with bound and checksum checks.
gtin ::
  forall i n.
  ((TN.<=) n 19, Integral i, KnownNat n) =>
  -- | An 'Integral' value for which we want to construct a 'GTIN' number.
  i ->
  -- | A 'GTIN' number wrapped in a 'Just' if the given value is within bounds and the checksum matches; 'Nothing' otherwise.
  Maybe (GTIN n)
gtin v''
  | 0 <= v' && v' <= m && checkChecksum v = Just v
  | otherwise = Nothing
  where
    v' = fromIntegral v'' :: Integer
    v = GTIN (fromIntegral v')
    m = _maxBound (error "should not be evaluated" :: GTIN n)

_decw :: KnownNat n => GTIN n -> Int
_decw = fromIntegral . natVal

_maxBound :: (Integral i, KnownNat n) => GTIN n -> i
_maxBound = pred . (10 ^) . _decw

_checkgtin :: Word64 -> GTIN n
_checkgtin = fixChecksum . GTIN

_checkgtin' :: [Word64] -> [GTIN n]
_checkgtin' = map _checkgtin

_wipe :: Word64 -> Word64
_wipe w = w - w `mod` 10

_tocheck :: Integral i => i -> i -> i
_tocheck n d = (d + n1 + 3 * n2) `mod` 10
  where
    ~(n1, n2) = n `quotRem` 10

_determineChecksum :: Word64 -> Word64
_determineChecksum w = (10 - go w 0) `mod` 10
  where
    go 0 = id
    go n = go q . _tocheck r
      where
        ~(q, r) = n `quotRem` 100

-- | Fix the checksum of a given 'GTIN' object. If the checksum is valid, then it will return the same GTIN, this operation is thus /idempotent/.
fixChecksum ::
  -- | The given 'GTIN' number where we fix the checksum from.
  GTIN n ->
  -- | A 'GTIN' object that is the variant of the given 'GTIN' number, with a valid checksum.
  GTIN n
fixChecksum (GTIN w') = GTIN (w' - w1 + _determineChecksum w0)
  where
    ~(w0, w1) = w' `quotRem` 10

-- | Check if the given checksum matches.
checkChecksum ::
  -- | The given 'GTIN' number for which we check the checksum.
  GTIN n ->
  -- | 'True' if the given checksum matches; 'False' otherwise.
  Bool
checkChecksum (GTIN w') = _determineChecksum w0 == w1
  where
    ~(w0, w1) = w' `quotRem` 10

-- upscaleGTIN :: m TN.<= n => GTIN m -> GTIN n
-- upscaleGTIN (GTIN w) = GTIN w

-- | Check if two 'GTIN' numbers, possibly with a different "width" are equivalent.
equivGTIN ::
  -- | The first 'GTIN' to check.
  GTIN m ->
  -- | The second 'GTIN' to check.
  GTIN n ->
  -- | 'True' if the given 'GTIN' values are equivalent; 'False' otherwise.
  Bool
equivGTIN (GTIN w1) (GTIN w2) = w1 == w2

instance KnownNat n => Validity (GTIN n) where
  validate g@(GTIN w) =
    check (w <= _maxBound g) "The value is larger than the maximum number of digits."
      `mappend` check (checkChecksum g) "checksum does not match."

instance KnownNat n => Show (GTIN n) where
  showsPrec d g@(GTIN v) = showParen (d > 0) (("GTIN " ++ printf ("%0" ++ sn ++ "d") v ++ " :: GTIN " ++ sn) ++)
    where
      sn = show (_decw g)

-- | Convert the given 'GTIN' number to convert to a 'String' that groups numbers into groups of four.
gtinToString ::
  KnownNat n =>
  -- | The given 'GTIN' number to convert to a readable 'String'.
  GTIN n ->
  -- | A 'String' that contains the GTIN number, in chucks of four digits.
  String
gtinToString g@(GTIN w) = unwords (map p (reverse (unfoldr f (n, w))))
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

#if MIN_VERSION_base(4,16,4)
instance KnownNat n => Bounded (GTIN (n :: Natural)) where
#else
instance KnownNat n => Bounded (GTIN (n :: Nat)) where
#endif
  minBound = fixChecksum (GTIN 0)
  maxBound = fixChecksum (GTIN (10 ^ _decw (error "should not be evaluated" :: GTIN n) - 1))

#if MIN_VERSION_base(4,16,4)
instance KnownNat n => Enum (GTIN (n :: Natural)) where
#else
instance KnownNat n => Enum (GTIN (n :: Nat)) where
#endif
  succ (GTIN w) = fixChecksum (GTIN (w + 10))
  pred (GTIN w) = fixChecksum (GTIN (w - 10))
  toEnum = GTIN . toEnum
  fromEnum (GTIN w) = fromEnum w
  enumFrom g@(GTIN n) = _checkgtin' [n, n + 10 .. _maxBound g]
  enumFromThen g@(GTIN m) (GTIN n)
    | m <= n = _checkgtin' [_wipe m, _wipe n .. _maxBound g]
    | otherwise = _checkgtin' [_wipe m, _wipe n .. 0]
  enumFromThenTo (GTIN m) (GTIN n) (GTIN o) = _checkgtin' [_wipe m, _wipe n .. _wipe o]
  enumFromTo (GTIN m) (GTIN n) = map (fixChecksum . GTIN) [m, m + 10 .. n]

-- | A type alias for a 'GTIN' number with fourteen numbers, with as range @00 0000 0000 0000@–@99 9999 9999 9997@.
type GTIN14 = GTIN 14

-- | A type alias for a 'GTIN' number with thirteen numbers, with as range @0 0000 0000 0000@–@9 9999 9999 9994@.
type GTIN13 = GTIN 13

-- | A type alias for a 'GTIN' number with twelve numbers, with as range @0000 0000 0000@–@9999 9999 9993@.
type GTIN12 = GTIN 12

-- | A type alias for a 'GTIN' number with eight numbers, with as range @0000 0000@–@9999 9995@.
type GTIN8 = GTIN 8

-- | A type alias for a 'GTIN' number with seventeen numbers, with as range @0 0000 0000 0000 0000@–@9 9999 9999 9999 9992@.
type GSIN = GTIN 17

-- | A type alias for a 'GTIN' number with eighteen numbers, with as range @00 0000 0000 0000 0000@–@99 9999 9999 9999 9995@.
type SSCC = GTIN 18

-- | A type alias for a 'GTIN' number with fourteen numbers, with as range @00 0000 0000 0000@–@99 9999 9999 9997@.
type EANUCC14 = GTIN14

-- | A type alias for a 'GTIN' number with fourteen numbers, with as range @00 0000 0000 0000@–@99 9999 9999 9997@.
type SCC14 = GTIN14

-- | A type alias for a 'GTIN' number with thirteen numbers, with as range @0 0000 0000 0000@–@9 9999 9999 9994@.
type EAN = GTIN13

-- | A type alias for a 'GTIN' number with thirteen numbers, with as range @0 0000 0000 0000@–@9 9999 9999 9994@.
type EANUCC13 = GTIN13

-- | A type alias for a 'GTIN' with thirtheen numbers which is also an ISBN number, with as range @0 0000 0000 0000@–@9 9999 9999 9994@.
type ISBN = GTIN13

-- | A type alias for a 'GTIN' with thirtheen numbers which is also an ISBN number, with as range @0 0000 0000 0000@–@9 9999 9999 9994@.
type ISBN13 = GTIN13

-- | A type alias for a 'GTIN' number with eight numbers, with as range @0000 0000@–@9999 9995@.
type EANUCC8 = GTIN8
