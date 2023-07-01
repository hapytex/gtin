{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
#if !MIN_VERSION_base(4,17,1)
{-# LANGUAGE TypeFamilies #-}
#endif
{-# OPTIONS_GHC -Wall -fno-warn-redundant-constraints #-}

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
    upscaleGTIN,

    -- * Fix the checksum of a GTIN number
    fixChecksum,
    checkChecksum,

    -- * Convert the GTINs to a readable format.
    gtinToString,

    -- * ISBN-10 to ISBN-13
    fromISBN10',

    -- * Parsing GTINs
    gtinParser,
    gtinParser_,
    gtinParser',
    gtinParser_',
    parseGTIN,
    parseGTIN_,
    parseGTIN',
    parseGTIN_',

    -- * QuasiQuoters
    gtinQ,
    gtin14Q,
    gtin13Q,
    gtin12Q,
    gtin8Q,
    eanucc8Q,
    eanucc14Q,
    scc14Q,
    eanQ,
    eanucc13Q,
    gsinQ,
    ssccQ,
    isbnQ,
    isbn13Q,
  )
where

import Control.Monad ((>=>))
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail(MonadFail)
#endif
import Data.Binary (Binary (get, put))
import Data.Char (chr, digitToInt)
import Data.Data (Data)
import Data.Functor.Identity (Identity)
import Data.Hashable (Hashable)
import Data.List (unfoldr)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)
#if MIN_VERSION_validity(0,9,0)
import Data.Validity (Validity (validate), check, prettyValidate)
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
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (choose)
import Text.Parsec (ParseError)
import Text.Parsec.Char (digit, space)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Prim (ParsecT, Stream, runParser, skipMany)
import Text.Printf (PrintfArg, printf)

#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH.Syntax (Code (Code), Exp (AppE, ConE, LitE), Lift (lift, liftTyped), Lit (IntegerL), Pat (ConP, LitP), TExp (TExp))
#elif MIN_VERSION_template_haskell(2, 16, 0)
import Language.Haskell.TH.Syntax (Exp (AppE, ConE, LitE), Lift (lift, liftTyped), Lit (IntegerL), Pat (ConP, LitP), TExp (TExp))
#else
import Language.Haskell.TH.Syntax (Exp (AppE, ConE, LitE), Lift (lift), Lit (IntegerL), Pat (ConP, LitP))
#endif

#if MIN_VERSION_base(4,16,4)
-- | A datatype for /Global Trade Item Numbers 'GTIN'/ with arbitrary "width" (up to nineteen digits technically possible).
newtype GTIN (n :: Natural) = GTIN Word64 deriving (Data, Eq, Generic, Ord, Read, Typeable)
#else
-- | A datatype for /Global Trade Item Numbers 'GTIN'/ with arbitrary "width" (up to nineteen digits technically possible).
newtype GTIN (n :: Nat) = GTIN Word64 deriving (Data, Eq, Generic, Ord, Read, Typeable)
#endif

_hole :: GTIN n
_hole = error "should not be evaluated"

_fromEnum :: GTIN n -> Word64
_fromEnum (GTIN w) = w `div` 10

_toEnum :: Word64 -> GTIN n
_toEnum = fixChecksum . GTIN . (10 *)

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
    m = _maxBound (_hole :: GTIN n)

_decw :: KnownNat n => GTIN n -> Int
_decw = fromIntegral . natVal

_decw' :: KnownNat n => GTIN n -> Int
_decw' = pred . _decw

_maxBound' :: (Integral i, KnownNat n) => GTIN n -> i
_maxBound' = (10 ^) . _decw

_maxBound'' :: (Integral i, KnownNat n) => GTIN n -> i
_maxBound'' = (10 ^) . _decw'

_maxBound :: (Integral i, KnownNat n) => GTIN n -> i
_maxBound = pred . _maxBound'

-- | values without checksum digit (so divided by ten)
_modBound :: (Integral i, KnownNat n) => GTIN n -> i -> i
_modBound = flip mod . _maxBound''

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

-- | Convert one 'GTIN' into a 'GTIN' that has more digits. The new 'GTIN' will have additional leading zeros.
upscaleGTIN ::
  (TN.<=) m n =>
  -- | The original 'GTIN' number to upscale.
  GTIN m ->
  -- | A 'GTIN' with the same number, but more (or the same) number of digits.
  GTIN n
upscaleGTIN (GTIN w) = GTIN w

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
      `mappend` check (checkChecksum g) ("checksum does not match: expected " ++ pf ++ cc c' : ", but got " ++ pf ++ cc w0 : ".")
    where
      ~(w', w0) = w `divMod` 10
      c' = _determineChecksum w'
      pf = _printf (_decw' g) w'
      cc = chr . (0x1d7ce +) . fromIntegral

_printf :: (Integral i, Show i, PrintfArg j) => i -> j -> String
_printf = printf . ("%0" ++) . (++ "d") . show

_printf' :: (KnownNat n, PrintfArg j) => GTIN n -> j -> String
_printf' = _printf . _decw

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
    ww' = n `div` 2
    ww = ww' - ww' `mod` 2
    p (n0, v) = printf ("%0" ++ show n0 ++ "d") v
    f (n0, v)
      | n0 <= 0 = Nothing
      | otherwise = Just ((dd, r), (n0 - dd, q))
      where
        ~(q, r) = v `quotRem` 10000
        dd = min ww n0

instance ((TN.<=) n 19, KnownNat n) => Num (GTIN n) where
  g1 + g2 = _toEnum (_modBound g1 (_fromEnum g1 + _fromEnum g2)) -- can handle overflow, since we first omit the checksum
  g1 - g2 = _toEnum (_modBound g1 (_maxBound'' g1 + _fromEnum g1 - _fromEnum g2))
  g1 * g2 = _toEnum (fromInteger (_modBound g1 (fe g1 * fe g2)))
    where
      fe = fromIntegral . _fromEnum
  negate g = _toEnum (_modBound g (_maxBound'' g - _fromEnum g))
  abs = id
  signum = _toEnum . signum . _fromEnum
  fromInteger w = v
    where
      v = _toEnum (fromInteger (_modBound v w))

instance ((TN.<=) n 19, KnownNat n) => Real (GTIN n) where
  toRational = toRational . _fromEnum

instance ((TN.<=) n 19, KnownNat n) => Integral (GTIN n) where
  toInteger = toInteger . _fromEnum
  g1 `quotRem` g2 = (_toEnum q, _toEnum r)
    where
      ~(q, r) = _fromEnum g1 `quotRem` _fromEnum g2
  g1 `divMod` g2 = (_toEnum d, _toEnum m)
    where
      ~(d, m) = _fromEnum g1 `divMod` _fromEnum g2
  g1 `quot` g2 = _toEnum (_fromEnum g1 `quot` _fromEnum g2)
  g1 `rem` g2 = _toEnum (_fromEnum g1 `rem` _fromEnum g2)
  g1 `div` g2 = _toEnum (_fromEnum g1 `div` _fromEnum g2)
  g1 `mod` g2 = _toEnum (_fromEnum g1 `mod` _fromEnum g2)

instance ((TN.<=) n 19, KnownNat n) => Arbitrary (GTIN n) where
  arbitrary = _toEnum <$> choose (0, _maxBound'' (_hole :: GTIN n) - 1)

instance Hashable (GTIN n)

instance Binary (GTIN n) where
  get = GTIN <$> get
  put (GTIN w) = put w

#if MIN_VERSION_base(4,16,4)
instance KnownNat n => Bounded (GTIN (n :: Natural)) where
  minBound = fixChecksum (GTIN 0)
  maxBound = fixChecksum (GTIN (10 ^ _decw (_hole :: GTIN n) - 1))
#else
instance KnownNat n => Bounded (GTIN (n :: Nat)) where
  minBound = fixChecksum (GTIN 0)
  maxBound = fixChecksum (GTIN (10 ^ _decw (_hole :: GTIN n) - 1))
#endif

#if MIN_VERSION_base(4,16,4)
instance KnownNat n => Enum (GTIN (n :: Natural)) where
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
#else
instance KnownNat n => Enum (GTIN (n :: Nat)) where
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
#endif

instance Lift (GTIN n) where
  lift (GTIN w) = pure (ConE 'GTIN `AppE` LitE (IntegerL (fromIntegral w)))

#if MIN_VERSION_template_haskell(2, 17, 0)
  liftTyped (GTIN w) = Code (pure (TExp (ConE 'GTIN `AppE` (LitE (IntegerL (fromIntegral w))))))
#elif MIN_VERSION_template_haskell(2, 16, 0)
  liftTyped (GTIN w) = pure (TExp (ConE 'GTIN `AppE` (LitE (IntegerL (fromIntegral w)))))
#endif

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

-- | Convert a given integral number that contains an ISBN-10 number into the 'ISBN13' equivalent. For example @8175257660@ is converted to @9 7881 7525 7665@. This will add a @978@ prefix,
-- and recalculate the checksum.
fromISBN10' ::
  Integral i =>
  -- | An 'Integral' number that contains an ISBN-10.
  i ->
  -- | The equivalent ISBN-13 number, which is a 'GTIN' number with the corresponding checksum algorithm.
  ISBN13
fromISBN10' = fixChecksum . GTIN . (9780000000000 +) . fromIntegral

#if !MIN_VERSION_validity(0,9,0)
prettyValidate :: Validity a => a -> Either String a
prettyValidate a = go (validate a)
  where go (Validation []) = Right a
        go v = Left (show v)
#endif

#if MIN_VERSION_template_haskell(2, 18, 0)
_toPattern :: GTIN n -> Pat
_toPattern (GTIN w) = ConP 'GTIN [] [LitP (IntegerL (fromIntegral w))]
#else
_toPattern :: GTIN n -> Pat
_toPattern (GTIN w) = ConP 'GTIN [LitP (IntegerL (fromIntegral w))]
#endif

_liftEither :: (Show s, MonadFail m) => Either s a -> m a
_liftEither = either (fail . show) pure

-- | A parser for a gtin number with an arbitrary number of digits between two and nineteen. the parser does not /end/ after the gtin (so no 'eof' is required),
-- and furthermore does /not/ validate if the gtin is indeed valid. The parser parses the number of digits with an arbitrary number of spaces between any two digits.
gtinParser_' ::
  forall s u m n.
  ((TN.<=) 2 n, (TN.<=) n 19, KnownNat n, Stream s m Char) =>
  -- | A parser parsing a GTIN with an arbitrary number of digits that does /not/ force the stream to end, and does /not/ check the checksum.
  ParsecT s u m (GTIN n)
gtinParser_' = GTIN <$> (dd >>= go (_decw' (_hole :: GTIN n)))
  where
    go 0 v = pure v
    go n v = (skipMany space *> dd) >>= go (n - 1) . ((10 * v) +)
    dd = fromIntegral . digitToInt <$> digit

-- | A parser for a gtin number with an arbitrary number of digits between two and nineteen. the parser does not /end/ after the gtin (so no 'eof' is required).
-- The GTIN is validated, so if the checksum does not match, the parser fails. The parser parses the number of digits with an arbitrary number of spaces between any two digits.
gtinParser_ ::
  forall s u m n.
  ((TN.<=) 2 n, (TN.<=) n 19, KnownNat n, Stream s m Char) =>
  -- | A parser parsing a GTIN with an arbitrary number of digits that does /not/ force the stream to end, but checks the checksum.
  ParsecT s u m (GTIN n)
gtinParser_ = gtinParser_' >>= _liftEither . prettyValidate

-- | A parser for a gtin number with an arbitrary number of digits between two and nineteen. the parser forces the stream to /end/ after the gtin,
-- but does /not/ validate if the gtin is indeed valid. The parser parses the number of digits with an arbitrary number of spaces between any two digits.
gtinParser' ::
  forall s u m n.
  ((TN.<=) 2 n, (TN.<=) n 19, KnownNat n, Stream s m Char) =>
  -- | A parser parsing a GTIN with an arbitrary number of digits thats force the stream to end, but does /not/ check the checksum.
  ParsecT s u m (GTIN n)
gtinParser' = gtinParser_' <* eof

-- | A parser for a gtin number with an arbitrary number of digits between two and nineteen. the parser forces the stream to /end/ after the gtin,
-- and validates if the gtin is indeed valid. The parser parses the number of digits with an arbitrary number of spaces between any two digits.
gtinParser ::
  forall s u m n.
  ((TN.<=) 2 n, (TN.<=) n 19, KnownNat n, Stream s m Char) =>
  -- | A parser parsing a GTIN with an arbitrary number of digits that forces the stream to end, and does checks the checksum.
  ParsecT s u m (GTIN n)
gtinParser = gtinParser_ <* eof

-- | Run the 'gtinParser_'' parser and thus parses a 'GTIN' with an arbitrary number of digits. The parser does not require the stream to end after the 'GTIN', and does /not/ validate the checksum.
parseGTIN_' ::
  forall n s.
  ((TN.<=) 2 n, (TN.<=) n 19, KnownNat n, Stream s Identity Char) =>
  -- | The stream to parse.
  s ->
  -- | The result of the parser: 'Either' a 'ParseError' or the parsed 'GTIN'.
  Either ParseError (GTIN n)
parseGTIN_' = runParser gtinParser_' () ""

-- | Run the 'gtinParser_'' parser and thus parses a 'GTIN' with an arbitrary number of digits. The parser requires the stream to end after the 'GTIN', but does /not/ validate the checksum.
parseGTIN' ::
  forall n s.
  ((TN.<=) 2 n, (TN.<=) n 19, KnownNat n, Stream s Identity Char) =>
  -- | The stream to parse.
  s ->
  -- | The result of the parser: 'Either' a 'ParseError' or the parsed 'GTIN'.
  Either ParseError (GTIN n)
parseGTIN' = runParser gtinParser' () ""

-- | Run the 'gtinParser_'' parser and thus parses a 'GTIN' with an arbitrary number of digits. The parser does not require the stream to end after the 'GTIN', but validates the checksum.
parseGTIN_ ::
  forall n s.
  ((TN.<=) 2 n, (TN.<=) n 19, KnownNat n, Stream s Identity Char) =>
  -- | The stream to parse.
  s ->
  -- | The result of the parser: 'Either' a 'ParseError' or the parsed 'GTIN'.
  Either ParseError (GTIN n)
parseGTIN_ = runParser gtinParser_ () ""

-- | Run the 'gtinParser_'' parser and thus parses a 'GTIN' with an arbitrary number of digits. The parser requires the stream to end after the 'GTIN', and validates the checksum.
parseGTIN ::
  forall n s.
  ((TN.<=) 2 n, (TN.<=) n 19, KnownNat n, Stream s Identity Char) =>
  -- | The stream to parse.
  s ->
  -- | The result of the parser: 'Either' a 'ParseError' or the parsed 'GTIN'.
  Either ParseError (GTIN n)
parseGTIN = runParser gtinParser () ""

-- | A function that constructs a 'GTIN' expression or pattern based on a given string. The 'Proxy' parameter is used to specify the number of digits of the 'GTIN' number.
gtinQ ::
  forall n.
  ((TN.<=) 2 n, (TN.<=) n 19, KnownNat n) =>
  -- | The 'Proxy' object that is ignored, but used to determine the number of digits of the 'GTIN' number.
  Proxy (GTIN n) ->
  -- | The corresponding 'QuasiQuoter' that resolves to a 'GTIN' expression or pattern.
  QuasiQuoter
gtinQ _ =
  QuasiQuoter
    { quoteExp = (_liftEither >=> lift) . parseGTIN @n,
      quotePat = (_liftEither >=> pure . _toPattern) . parseGTIN @n,
      quoteType = const (fail "can not produce a type with this QuasiQuoter"),
      quoteDec = const (fail "can not produce a declaration with this QuasiQuoter")
    }

-- | A 'QuasiQuoter' for an 'SSCC' number.
ssccQ ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
ssccQ = gtinQ @18 Proxy

-- | A 'QuasiQuoter' for a 'GSIN' number.
gsinQ ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
gsinQ = gtinQ @17 Proxy

-- | A 'QuasiQuoter' for an 'GTIN14' number.
gtin14Q ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
gtin14Q = gtinQ @14 Proxy

-- | A 'QuasiQuoter' for an 'EANUCC14' number.
eanucc14Q ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
eanucc14Q = gtin14Q

-- | A 'QuasiQuoter' for an 'SCC14' number.
scc14Q ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
scc14Q = gtin14Q

-- | A 'QuasiQuoter' for an 'GTIN13' number.
gtin13Q ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
gtin13Q = gtinQ @13 Proxy

-- | A 'QuasiQuoter' for an 'EAN' number.
eanQ ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
eanQ = gtin13Q

-- | A 'QuasiQuoter' for an 'EANUCC13' number.
eanucc13Q ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
eanucc13Q = gtin13Q

-- | A 'QuasiQuoter' for an 'GTIN12' number.
gtin12Q ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
gtin12Q = gtinQ @12 Proxy

-- | A 'QuasiQuoter' for an 'GTIN8' number.
gtin8Q ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
gtin8Q = gtinQ @8 Proxy

-- | A 'QuasiQuoter' for an 'EANUCC8' number.
eanucc8Q ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
eanucc8Q = gtin8Q

-- | A 'QuasiQuoter' for an 'ISBN13' number.
isbn13Q ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
isbn13Q = gtinQ @13 Proxy

-- | A 'QuasiQuoter' for an 'ISBN' number, only the thirteen digit number is supported.
isbnQ ::
  -- | The corresponding 'QuasiQuoter'.
  QuasiQuoter
isbnQ = isbn13Q
