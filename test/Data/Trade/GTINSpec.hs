{-# LANGUAGE QuasiQuotes, ExplicitForAll, TypeApplications, DataKinds, ScopedTypeVariables, KindSignatures, TypeFamilies #-}

module Data.Trade.GTINSpec where

import Data.Binary(decode, encode)
import Data.Proxy(Proxy(Proxy))
import Data.Validity(isValid)

import Data.Trade.GTIN(GTIN, GTIN8, GTIN13, gtin8Q, gtin13Q)

import qualified GHC.TypeNats as TN
import GHC.TypeNats (KnownNat)

import Test.Hspec(Spec, describe, it)
import Test.QuickCheck(maxSuccess, property, quickCheckWith, stdArgs)

associativePlus :: forall n . ((TN.<=) n 19, KnownNat n) => GTIN n -> GTIN n -> GTIN n -> Bool
associativePlus x y z = (x + y) + z == x + (y + z)

commutativePlus :: forall n . ((TN.<=) n 19, KnownNat n) => GTIN n -> GTIN n -> Bool
commutativePlus x y = x + y == y + x

additiveIdentity :: forall n . ((TN.<=) n 19, KnownNat n) => GTIN n -> Bool
additiveIdentity x = x + fromInteger 0 == x

negateAdditiveInverse :: forall n . ((TN.<=) n 19, KnownNat n) => GTIN n -> Bool
negateAdditiveInverse x = x + negate x == fromInteger 0

associativeMultiply :: forall n . ((TN.<=) n 19, KnownNat n) => GTIN n -> GTIN n -> GTIN n -> Bool
associativeMultiply x y z = (x * y) * z == x * (y * z)

multiplicativeIdentity :: forall n . ((TN.<=) n 19, KnownNat n) => GTIN n -> Bool
multiplicativeIdentity x = x * fromInteger 1 == x

distributivityOfMultiplyWrtPlus :: forall n . ((TN.<=) n 19, KnownNat n) => GTIN n -> GTIN n -> GTIN n -> Bool
distributivityOfMultiplyWrtPlus x y z = x * (y + z) == x * y + x * z && (y + z) * x == y * x + z * x

coherenceFromInteger :: forall n . ((TN.<=) n 19, KnownNat n) => GTIN n -> Bool
coherenceFromInteger x = fromInteger (toInteger x) == x

identityEncodingDecoding :: forall n . GTIN n -> Bool
identityEncodingDecoding x = decode (encode x) == x

{-
eqReflexivity :: forall n. GTIN n -> Bool
eqReflexivity x = x == x

eqSymmetry :: forall n. GTIN n -> GTIN n -> Bool
eqSymmetry x y = (x == y) == (y == x)

eqTransitivity :: forall n. GTIN n -> GTIN n -> GTIN n -> Bool
eqTransitivity x y z = not (x == y && y == z) || x == z

eqExtensionality :: forall a b. (Eq a, Eq b) => Fun a b -> a -> a -> Bool
eqExtensionality f x y = x /= y || applyFun f x == applyFun f y

eqNegation :: forall a. Eq a => a -> a -> Bool
eqNegation x y = (x /= y) == not (x == y)
-}

eqCheck :: forall n . ((TN.<=) n 19, KnownNat n) => Proxy n -> Spec
eqCheck _ =
  describe "instance Eq GTIN" $ do
    it "associativity of (+)" (property (associativePlus @n))
    it "commutativity of (+)" (property (commutativePlus @n))
    it "fromInteger 0 is the additive identity" (property (additiveIdentity @n))
    it "negate gives the additive inverse" (property (negateAdditiveInverse @n))
    it "associativity of (*)" (property (associativeMultiply @n))
    it "fromInteger 1 is the multiplicative identity" (property (multiplicativeIdentity @n))
    it "distributivity of (*) with respect to (+)" (property (distributivityOfMultiplyWrtPlus @n))


numCheck :: forall n . ((TN.<=) n 19, KnownNat n) => Proxy n -> Spec
numCheck _ =
  describe "instance Num GTIN" $ do
    it "associativity of (+)" (property (associativePlus @n))
    it "commutativity of (+)" (property (commutativePlus @n))
    it "fromInteger 0 is the additive identity" (property (additiveIdentity @n))
    it "negate gives the additive inverse" (property (negateAdditiveInverse @n))
    it "associativity of (*)" (property (associativeMultiply @n))
    it "fromInteger 1 is the multiplicative identity" (property (multiplicativeIdentity @n))
    it "distributivity of (*) with respect to (+)" (property (distributivityOfMultiplyWrtPlus @n))

integralCheck :: forall n . ((TN.<=) n 19, KnownNat n) => Proxy n -> Spec
integralCheck _ = describe "instance Integral GTIN" (it "coherence with fromInteger" (property (coherenceFromInteger @n)))

binaryCheck :: forall n .((TN.<=) n 19, KnownNat n) => Proxy n -> Spec
binaryCheck _ = describe "instance Binary GTIN" (it "identity of encoding and decoding" (property (identityEncodingDecoding @n)))

checks :: forall n . ((TN.<=) n 19, KnownNat n) => Proxy n -> Spec
checks p = eqCheck p >> numCheck p >> integralCheck p >> binaryCheck p

spec :: Spec
spec = do
  describe "GTIN2" (checks @2 Proxy)
  describe "GTIN3" (checks @3 Proxy)
  describe "GTIN4" (checks @4 Proxy)
  describe "GTIN5" (checks @5 Proxy)
  describe "GTIN6" (checks @6 Proxy)
  describe "GTIN7" (checks @7 Proxy)
  describe "GTIN8" (checks @8 Proxy)
  describe "GTIN9" (checks @9 Proxy)
  describe "GTIN10" (checks @10 Proxy)
  describe "GTIN11" (checks @11 Proxy)
  describe "GTIN12" (checks @12 Proxy)
  describe "GTIN13" (checks @13 Proxy)
  describe "GTIN14" (checks @14 Proxy)
  describe "GTIN15" (checks @15 Proxy)
  describe "GTIN16" (checks @16 Proxy)
  describe "GTIN17" (checks @17 Proxy)
  describe "GTIN18" (checks @18 Proxy)
  describe "GTIN19" (checks @19 Proxy)

example :: GTIN8 -> GTIN13
example [gtin8Q|12345670|] = [gtin13Q|1234567890128|]
example _ = [gtin13Q|0000000000000|]
