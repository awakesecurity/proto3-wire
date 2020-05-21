{-
  Copyright 2020 Awake Networks

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

-- | Implementation details of the "Data.ByteString.Reverse" module.
-- Breaking changes will be more frequent in this module; use with caution.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module Proto3.Wire.Reverse.Prim
  ( StoreMethod(..)
  , storeMethod
  , ByteOrder(..)
  , systemByteOrder
  , BoundedPrimR(..)
  , composeBoundedPrimR
  , unsafeBoundedPrimR
  , primBoundedR
  , FixedPrimR
  , liftFixedToBoundedR
  , primPoke
  , word8
  , word16
  , word16Native
  , word16BE
  , word16LE
  , word32
  , word32Native
  , word32BE
  , word32LE
  , word64
  , word64Native
  , word64BE
  , word64LE
  , int8
  , int16
  , int16Native
  , int16BE
  , int16LE
  , int32
  , int32Native
  , int32BE
  , int32LE
  , int64
  , int64Native
  , int64BE
  , int64LE
  , float
  , floatNative
  , floatBE
  , floatLE
  , double
  , doubleNative
  , doubleBE
  , doubleLE
  , charUtf8
  , wordBase128LEVar
  , wordBase128LEVar_inline
  , word32Base128LEVar
  , word32Base128LEVar_inline
  , word64Base128LEVar
  , word64Base128LEVar_inline
  , vectorFixedPrimR
  ) where

import           Data.Bits                     ( Bits(..) )
import           Data.Bool                     ( bool )
import           Data.Char                     ( ord )
import           Data.Int                      ( Int8, Int16, Int32, Int64 )
import           Data.Kind                     ( Type )
import qualified Data.Vector.Generic
import           Data.Word                     ( Word16,
                                                 byteSwap16, byteSwap32,
                                                 byteSwap64 )
import           Foreign                       ( Storable(..) )
import           GHC.Exts                      ( Addr#, Int#, Proxy#,
                                                 RealWorld, State#, (+#),
                                                 and#, inline, or#,
                                                 plusAddr#, plusWord#, proxy#,
                                                 uncheckedShiftRL# )
import           GHC.IO                        ( IO(..) )
import           GHC.Int                       ( Int(..) )
import           GHC.Ptr                       ( Ptr(..) )
import           GHC.TypeLits                  ( KnownNat, Nat,
                                                 type (+), natVal' )
import           GHC.Types                     ( Char(..), Double(..),
                                                 Float(..) )
import           GHC.Word                      ( Word(..), Word8(..),
                                                 Word32(..), Word64(..) )
import           Proto3.Wire.Reverse.Internal
import           Proto3.Wire.Reverse.Width     ( SemigroupNat(..),
                                                 MonoidNat(..),
                                                 Max, ChooseNat(..) )

#include <MachDeps.h>  /* for WORDS_BIGENDIAN and WORD_SIZE_IN_BITS */

-- "ghc-prim" v0.6.1 defines `GHC.Prim.Ext.WORD64`, but we do not wish
-- to require that version of "ghc-prim".  Therefore we define it locally.
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64 (Word64#)
type WORD64 = Word64#
#else
import GHC.Exts (Word#)
type WORD64 = Word#
#endif

-- $setup
-- >>> :set -XOverloadedStrings

-- | Are we restricted to aligned writes only?
data StoreMethod = StoreAligned | StoreUnaligned
  deriving (Eq, Show)

-- The Cabal file defines UNALIGNED_POKES on architectures where that
-- approach is known to be safe and faster then writing bytes one by one.
storeMethod :: StoreMethod
#if defined(UNALIGNED_POKES)
storeMethod = StoreUnaligned
#else
storeMethod = StoreAligned
#endif

data ByteOrder = BigEndian | LittleEndian
  deriving (Eq, Show)

-- WORDS_BIGENDIAN is defined for big-endian architectures
-- by the GHC header <MachDeps.h>.
systemByteOrder :: ByteOrder
#if defined(WORDS_BIGENDIAN)
systemByteOrder = BigEndian
#else
systemByteOrder = LittleEndian
#endif

-- | A 'BuildR' together with a type-level bound on the number of bytes
-- written and a requirement that the current buffer already contain at
-- least that many bytes.
--
-- As in the "bytestring" package, the purpose of a bounded primitive is to
-- improve speed by consolidating the space checks of several small builders.
newtype BoundedPrimR (w :: Nat) = BoundedPrimR BuildR

type role BoundedPrimR nominal

instance SemigroupNat BoundedPrimR
  where
    (>+<) = composeBoundedPrimR
    {-# INLINE CONLIKE (>+<) #-}

    assocLPlusNat p = assocLPlusNatBoundedPrimR p
    {-# INLINE CONLIKE assocLPlusNat #-}

    assocRPlusNat p = assocRPlusNatBoundedPrimR p
    {-# INLINE CONLIKE assocRPlusNat #-}

    commPlusNat _ (BoundedPrimR f) = BoundedPrimR f
    {-# INLINE CONLIKE commPlusNat #-}

instance MonoidNat BoundedPrimR
  where
    memptyNat = BoundedPrimR mempty
    {-# INLINE CONLIKE memptyNat #-}

instance ChooseNat BoundedPrimR
  where
    boolNat = \(BoundedPrimR f) (BoundedPrimR g) -> BoundedPrimR . bool f g
    {-# INLINE CONLIKE boolNat #-}

    assocLMaxNat = \_ (BoundedPrimR f) -> BoundedPrimR f
    {-# INLINE CONLIKE assocLMaxNat #-}

    assocRMaxNat = \_ (BoundedPrimR f) -> BoundedPrimR f
    {-# INLINE CONLIKE assocRMaxNat #-}

    commMaxNat = \_ (BoundedPrimR f) -> BoundedPrimR f
    {-# INLINE CONLIKE commMaxNat #-}

-- | Like 'assocLPlusNat' but can be used in rules without
-- causing GHC to think the class dictionary is recursive.
assocLPlusNatBoundedPrimR ::
  forall u v w .
  Proxy# '(u, v, w) -> BoundedPrimR (u + (v + w)) -> BoundedPrimR ((u + v) + w)
assocLPlusNatBoundedPrimR = \_ (BoundedPrimR f) -> BoundedPrimR f
{-# INLINE CONLIKE assocLPlusNatBoundedPrimR #-}

-- | Like 'assocRPlusNat' but can be used in rules without
-- causing GHC to think the class dictionary is recursive.
assocRPlusNatBoundedPrimR ::
  forall u v w .
  Proxy# '(u, v, w) -> BoundedPrimR ((u + v) + w) -> BoundedPrimR (u + (v + w))
assocRPlusNatBoundedPrimR = \_ (BoundedPrimR f) -> BoundedPrimR f
{-# INLINE CONLIKE assocRPlusNatBoundedPrimR #-}

-- | Needed for rewrite rules; normally you would use '(>+<)'.
composeBoundedPrimR :: BoundedPrimR v -> BoundedPrimR w -> BoundedPrimR (v + w)
composeBoundedPrimR =
  \(BoundedPrimR f) (BoundedPrimR g) -> BoundedPrimR (f <> g)
{-# INLINE CONLIKE [1] composeBoundedPrimR #-}

-- | Executes the bounded primitive WITHOUT first ensuring it has enough space.
unsafeBoundedPrimR :: BoundedPrimR w -> BuildR
unsafeBoundedPrimR (BoundedPrimR build) = build

-- | Executes the given bounded primitive
-- after obtaining the space it requires.
primBoundedR :: forall w . KnownNat w => BoundedPrimR w -> BuildR
primBoundedR = case fromInteger (natVal' (proxy# :: Proxy# w)) of
  I# w -> unsafePrimBoundedR w
{-# INLINE CONLIKE primBoundedR #-}

-- | Needed for rewrite rules; normally you would use 'primBoundedR'.
unsafePrimBoundedR :: Int# -> BoundedPrimR w -> BuildR
unsafePrimBoundedR = \w (BoundedPrimR f) -> ensure# w f
{-# INLINE CONLIKE [1] unsafePrimBoundedR #-}

{-# RULES

"appendBuildR/unsafePrimBoundedR" forall w1 w2 f1 f2 .
    appendBuildR (unsafePrimBoundedR w1 f1) (unsafePrimBoundedR w2 f2)
  = unsafePrimBoundedR (w1 +# w2) (composeBoundedPrimR f1 f2)

"appendBuildR/unsafePrimBoundedR/assoc_r" forall w1 w2 f1 f2 b .
    appendBuildR (unsafePrimBoundedR w1 f1)
                 (appendBuildR (unsafePrimBoundedR w2 f2) b)
  = appendBuildR (unsafePrimBoundedR (w1 +# w2) (composeBoundedPrimR f1 f2)) b

"appendBuildR/unsafePrimBoundedR/assoc_l" forall w1 w2 f1 f2 b .
    appendBuildR (appendBuildR b (unsafePrimBoundedR w1 f1))
                 (unsafePrimBoundedR w2 f2)
  = appendBuildR b (unsafePrimBoundedR (w1 +# w2) (composeBoundedPrimR f1 f2))

  #-}

-- | Similar to a 'BoundedPrimR' but also consolidates address updates in
-- order to take advantage of machine instructions that write at an offset.
--
-- The additional input is an offset from the current address
-- that specifies the beginning of the region being encoded.
--
-- (If GHC learns to consolidate address offsets automatically
-- then we might be able to just use 'BoundedPrimR' instead.)
newtype FixedPrimR (w :: Nat) = FixedPrimR
  ( Addr# -> Int# -> State# RealWorld -> Int# ->
    (# Addr#, Int#, State# RealWorld #)
  )

type role FixedPrimR nominal

instance SemigroupNat FixedPrimR
  where
    (>+<) = \(FixedPrimR f :: FixedPrimR w) (FixedPrimR g) ->
      case fromInteger (natVal' (proxy# :: Proxy# w)) of
        I# w -> FixedPrimR
          ( \v0 u0 s0 o -> case g v0 u0 s0 (o +# w) of
             (# v1, u1, s1 #) -> f v1 u1 s1 o )
    {-# INLINE CONLIKE (>+<) #-}

    assocLPlusNat = \_ (FixedPrimR f) -> FixedPrimR f
    {-# INLINE CONLIKE assocLPlusNat #-}

    assocRPlusNat = \_ (FixedPrimR f) -> FixedPrimR f
    {-# INLINE CONLIKE assocRPlusNat #-}

    commPlusNat = \_ (FixedPrimR f) -> FixedPrimR f
    {-# INLINE CONLIKE commPlusNat #-}

instance MonoidNat FixedPrimR
  where
    memptyNat = FixedPrimR (\v u s _ -> (# v, u, s #))
    {-# INLINE CONLIKE memptyNat #-}

-- | Executes the given fixed primitive and adjusts the current address.
liftFixedToBoundedR :: forall w . KnownNat w => FixedPrimR w -> BoundedPrimR w
liftFixedToBoundedR = \(FixedPrimR f) -> BoundedPrimR (BuildR (g f))
  where
    !(I# o) = - fromInteger (natVal' (proxy# :: Proxy# w))
    g = \f v0 u0 s0 -> case f v0 u0 s0 o of
      (# v1, u1, s1 #) -> (# plusAddr# v1 o, u1 +# o, s1 #)
    {-# INLINE g #-}
{-# INLINE CONLIKE [1] liftFixedToBoundedR #-}

{-# RULES

"composeBoundedPrimR/liftFixedToBoundedR"
    forall (f1 :: KnownNat w1 => FixedPrimR w1)
           (f2 :: KnownNat (w1 + w2) => FixedPrimR w2).
    composeBoundedPrimR (liftFixedToBoundedR f1) (liftFixedToBoundedR f2)
  = liftFixedToBoundedR (f1 >+< f2)

"composeBoundedPrimR/liftFixedToBoundedR/assoc_r"
    forall (f1 :: KnownNat w1 => FixedPrimR w1)
           (f2 :: KnownNat (w1 + w2) => FixedPrimR w2)
           (b3 :: BoundedPrimR w3) .
    composeBoundedPrimR (liftFixedToBoundedR f1)
                        (composeBoundedPrimR (liftFixedToBoundedR f2) b3)
  = assocRPlusNatBoundedPrimR (proxy# :: Proxy# '(w1, w2, w3))
      (composeBoundedPrimR (liftFixedToBoundedR (f1 >+< f2)) b3)

"composeBoundedPrimR/liftFixedToBoundedR/assoc_l"
    forall (b1 :: BoundedPrimR w1)
           (f2 :: KnownNat w2 => FixedPrimR w2)
           (f3 :: KnownNat (w2 + w3) => FixedPrimR w3) .
    composeBoundedPrimR (composeBoundedPrimR b1 (liftFixedToBoundedR f2))
                        (liftFixedToBoundedR f3)
  = assocLPlusNatBoundedPrimR (proxy# :: Proxy# '(w1, w2, w3))
      (composeBoundedPrimR b1 (liftFixedToBoundedR (f2 >+< f3)))

"withLengthOf#/unsafePrimBoundedR/liftFixedToBoundedR" forall f w g .
    withLengthOf# f (unsafePrimBoundedR w (liftFixedToBoundedR g))
  = appendBuildR (f w) (unsafePrimBoundedR w (liftFixedToBoundedR g))

  #-}

-- | Required:
--
-- > fromInteger (natVal' (proxy# :: Proxy# (StorableWidth a))) =
-- >   sizeOf (undefined :: x)
type family StorableWidth (a :: Type) :: Nat

type instance StorableWidth Word8 = 1
type instance StorableWidth Word16 = 2
type instance StorableWidth Word32 = 4
type instance StorableWidth Word64 = 8

type instance StorableWidth Int8 = 1
type instance StorableWidth Int16 = 2
type instance StorableWidth Int32 = 4
type instance StorableWidth Int64 = 8

type instance StorableWidth Float = 4
type instance StorableWidth Double = 8

-- | WARNING: The write may be unaligned; check 'storeMethod' first.
primPoke :: Storable x => x -> FixedPrimR (StorableWidth x)
primPoke !x = FixedPrimR p
  where
    p v u s0 o =
      let IO q = pokeByteOff (Ptr v) (I# o) x
      in case q s0 of (# s1, (_ :: ()) #) -> (# v, u, s1 #)

-- | Fixed-width primitive that writes a single byte as-is.
word8 :: Word8 -> FixedPrimR 1
word8 = primPoke
  -- Byte order and alignment do not matter for a single byte.

-- | Shifts right by @s@ bits, then writes the least significant byte.
word8Shift :: Int -> Word -> FixedPrimR 1
word8Shift s x = word8 (fromIntegral (shiftR x s))

-- | Shifts right by @s@ bits, then writes the least significant 16-bit word.
word16Shift :: ByteOrder -> Int -> Word -> FixedPrimR 2
word16Shift bo = case bo of
    BigEndian    -> \(!s) (!x) -> p (s + h) x >+< p s x
    LittleEndian -> \(!s) (!x) -> p s x >+< p (s + h) x
  where
    h = 8
    p = word8Shift

-- | Writes the least significant 32-bit word, one byte at a time.
word32Shift :: ByteOrder -> Word -> FixedPrimR 4
word32Shift bo = case bo of
    BigEndian    -> \(!x) -> p h x >+< p 0 x
    LittleEndian -> \(!x) -> p 0 x >+< p h x
  where
    h = 16
    p = word16Shift bo

-- | Writes one byte at a time.
word64Shift :: ByteOrder -> Word64 -> FixedPrimR 8
word64Shift bo = case bo of
    BigEndian    -> \(!x) -> p (h x) >+< p x
    LittleEndian -> \(!x) -> p x >+< p (h x)
  where
    h x = shiftR x 32
    p = word32Shift bo . fromIntegral @Word64 @Word

-- | Fixed-width primitive that writes a 16-bit word
-- in the specified byte order.
word16 :: ByteOrder -> Word16 -> FixedPrimR 2
word16 !bo !x = case storeMethod of
  StoreAligned -> word16Shift bo 0 (fromIntegral x)
  StoreUnaligned
    | systemByteOrder == bo -> primPoke x
    | otherwise -> primPoke (byteSwap16 x)

-- | Fixed-width primitive that writes a 16-bit word
-- in native byte order.
word16Native :: Word16 -> FixedPrimR 2
word16Native = word16 systemByteOrder

-- | Fixed-width primitive that writes a 16-bit word
-- in big-endian byte order.
word16BE :: Word16 -> FixedPrimR 2
word16BE = word16 BigEndian

-- | Fixed-width primitive that writes a 16-bit word
-- in little-endian byte order.
word16LE :: Word16 -> FixedPrimR 2
word16LE = word16 LittleEndian

-- | Fixed-width primitive that writes a 32-bit word
-- in the specified byte order.
word32 :: ByteOrder -> Word32 -> FixedPrimR 4
word32 !bo !x = case storeMethod of
  StoreAligned -> word32Shift bo (fromIntegral x)
  StoreUnaligned
    | systemByteOrder == bo -> primPoke x
    | otherwise -> primPoke (byteSwap32 x)

-- | Fixed-width primitive that writes a 32-bit word
-- in native byte order.
word32Native :: Word32 -> FixedPrimR 4
word32Native = word32 systemByteOrder

-- | Fixed-width primitive that writes a 32-bit word
-- in big-endian byte order.
word32BE :: Word32 -> FixedPrimR 4
word32BE = word32 BigEndian

-- | Fixed-width primitive that writes a 32-bit word
-- in little-endian byte order.
word32LE :: Word32 -> FixedPrimR 4
word32LE = word32 LittleEndian

-- | Fixed-width primitive that writes a 64-bit word
-- in the specified byte order.
word64 :: ByteOrder -> Word64 -> FixedPrimR 8
word64 !bo !x = case storeMethod of
  StoreAligned -> word64Shift bo (fromIntegral x)
  StoreUnaligned
    | systemByteOrder == bo -> primPoke x
    | otherwise -> primPoke (byteSwap64 x)

-- | Fixed-width primitive that writes a 64-bit word
-- in native byte order.
word64Native :: Word64 -> FixedPrimR 8
word64Native = word64 systemByteOrder

-- | Fixed-width primitive that writes a 64-bit word
-- in big-endian byte order.
word64BE :: Word64 -> FixedPrimR 8
word64BE = word64 BigEndian

-- | Fixed-width primitive that writes a 64-bit word
-- in little-endian byte order.
word64LE :: Word64 -> FixedPrimR 8
word64LE = word64 LittleEndian

-- | @'word8' . 'fromIntegral'@
int8 :: Int8 -> FixedPrimR 1
int8 = word8 . fromIntegral

-- | @\bo -> 'word16' bo . 'fromIntegral'@
int16 :: ByteOrder -> Int16 -> FixedPrimR 2
int16 !bo = word16 bo . fromIntegral

-- | @'word16Native' . 'fromIntegral'@
int16Native :: Int16 -> FixedPrimR 2
int16Native = word16Native . fromIntegral

-- | @'word16BE' . 'fromIntegral'@
int16BE :: Int16 -> FixedPrimR 2
int16BE = word16BE . fromIntegral

-- | @'word16LE' . 'fromIntegral'@
int16LE :: Int16 -> FixedPrimR 2
int16LE = word16LE . fromIntegral

-- | @\bo -> 'word32' bo . 'fromIntegral'@
int32 :: ByteOrder -> Int32 -> FixedPrimR 4
int32 bo = word32 bo . fromIntegral

-- | @'word32Native' . 'fromIntegral'@
int32Native :: Int32 -> FixedPrimR 4
int32Native = word32Native . fromIntegral

-- | @'word32BE' . 'fromIntegral'@
int32BE :: Int32 -> FixedPrimR 4
int32BE = word32BE . fromIntegral

-- | @'word32LE' . 'fromIntegral'@
int32LE :: Int32 -> FixedPrimR 4
int32LE = word32LE . fromIntegral

-- | @\bo -> 'word64' bo . 'fromIntegral'@
int64 :: ByteOrder -> Int64 -> FixedPrimR 8
int64 bo = word64 bo . fromIntegral

-- | @'word64Native' . 'fromIntegral'@
int64Native :: Int64 -> FixedPrimR 8
int64Native = word64Native . fromIntegral

-- | @'word64BE' . 'fromIntegral'@
int64BE :: Int64 -> FixedPrimR 8
int64BE = word64BE . fromIntegral

-- | @'word64LE' . 'fromIntegral'@
int64LE :: Int64 -> FixedPrimR 8
int64LE = word64LE . fromIntegral

float :: ByteOrder -> Float -> FixedPrimR 4
float BigEndian = floatBE
float LittleEndian = floatLE

floatNative :: Float -> FixedPrimR 4
floatNative = float systemByteOrder

floatBE :: Float -> FixedPrimR 4
floatBE !x = FixedPrimR g
  where
    g v u s0 o = case floatToWord32 (Ptr v) (I# u) x of
      IO h -> case h s0 of
        (# s1, y #) ->
          let FixedPrimR f = word32BE y
          in f v u s1 o

floatLE :: Float -> FixedPrimR 4
floatLE !x = FixedPrimR g
  where
    g v u s0 o = case floatToWord32 (Ptr v) (I# u) x of
      IO h -> case h s0 of
        (# s1, y #) ->
          let FixedPrimR f = word32LE y
          in f v u s1 o

double :: ByteOrder -> Double -> FixedPrimR 8
double BigEndian = doubleBE
double LittleEndian = doubleLE

doubleNative :: Double -> FixedPrimR 8
doubleNative = double systemByteOrder

doubleBE :: Double -> FixedPrimR 8
doubleBE !x = FixedPrimR g
  where
    g v u s0 o = case doubleToWord64 (Ptr v) (I# u) x of
      IO h -> case h s0 of
        (# s1, y #) ->
          let FixedPrimR f = word64BE y
          in f v u s1 o

doubleLE :: Double -> FixedPrimR 8
doubleLE !x = FixedPrimR g
  where
    g v u s0 o = case doubleToWord64 (Ptr v) (I# u) x of
      IO h -> case h s0 of
        (# s1, y #) ->
          let FixedPrimR f = word64LE y
          in f v u s1 o

charUtf8 :: Char -> BoundedPrimR 4
charUtf8 = \ch -> case fromIntegral (ord ch) of W# x -> wordUtf8 x
  where
    wordUtf8 :: Word# -> BoundedPrimR 4
    wordUtf8 =
      choose 0x7F p1 $
      choose 0x7FF p2 $
      choose 0xFFFF p3 $
      (\y -> liftFixedToBoundedR (p4 y))
    {-# INLINE wordUtf8 #-}

    choose ::
      forall v w .
      (KnownNat v, KnownNat w) =>
      Word ->
      (Word# -> FixedPrimR v) ->
      (Word# -> BoundedPrimR w) ->
      Word# -> BoundedPrimR (Max w v)
    choose = \t f g x -> ifNat (W# x <= t) (liftFixedToBoundedR (f x)) (g x)
      -- We have observed GHC v8.6.5 jumping on the 'False' branch
      -- and falling through on the 'True' branch.  We set up our
      -- condition to favor lower character codes.
    {-# INLINE choose #-}

    lsb ::
      KnownNat n =>
      (Word# -> FixedPrimR n) ->
      Word# ->
      FixedPrimR (n + 1)
    lsb = \p x -> p (uncheckedShiftRL# x 6#) >+<
                  word8 (W8# (plusWord# 0x80## (and# x 0x3F##)))
    {-# INLINE lsb #-}

    p1 :: Word# -> FixedPrimR 1
    p2 :: Word# -> FixedPrimR 2
    p3 :: Word# -> FixedPrimR 3
    p4 :: Word# -> FixedPrimR 4

    p1 x = word8 (W8# x)
    p2 = lsb (\x -> word8 (W8# (plusWord# 0xC0## x)))
    p3 = lsb (lsb (\x -> word8 (W8# (plusWord# 0xE0## x))))
    p4 = lsb (lsb (lsb (\x -> word8 (W8# (plusWord# 0xF0## x)))))

    {-# INLINE p1 #-}
    {-# INLINE p2 #-}
    {-# INLINE p3 #-}
    {-# INLINE p4 #-}
{-# INLINE charUtf8 #-}

-- | The bounded primitive implementing
-- `Proto3.Wire.Reverse.wordBase128LEVar`.
#if WORD_SIZE_IN_BITS < 64
wordBase128LEVar :: Word -> BoundedPrimR 5
wordBase128LEVar (W# w) = word32Base128LEVar (W32# w)
#else
wordBase128LEVar :: Word -> BoundedPrimR 10
wordBase128LEVar (W# w) = word64Base128LEVar (W64# w)
#endif
{-# INLINE wordBase128LEVar #-}

-- | Like 'wordBase128LEVar' but inlined, possibly bloating your code.  On
-- the other hand, inlining an application to a constant may shrink your code.
#if WORD_SIZE_IN_BITS < 64
wordBase128LEVar_inline :: Word -> BoundedPrimR 5
wordBase128LEVar_inline (W# w) = word32Base128LEVar_inline (W32# w)
#else
wordBase128LEVar_inline :: Word -> BoundedPrimR 10
wordBase128LEVar_inline (W# w) = word64Base128LEVar_inline (W64# w)
#endif
{-# INLINE wordBase128LEVar_inline #-}

-- | The bounded primitive implementing
-- `Proto3.Wire.Reverse.word32Base128LEVar`.
word32Base128LEVar :: Word32 -> BoundedPrimR 5
word32Base128LEVar = word32Base128LEVar_inline
{-# INLINE word32Base128LEVar #-}

-- | Like 'word32Base128LEVar' but inlined, which currently means
-- that it is just the same as 'word32Base128LEVar', which we inline.
word32Base128LEVar_inline :: Word32 -> BoundedPrimR 5
word32Base128LEVar_inline = \(W32# x0) ->
  ( wordBase128LEVar_choose 1 wordBase128LE_p1 $
    wordBase128LEVar_choose 2 wordBase128LE_p2 $
    wordBase128LEVar_choose 3 wordBase128LE_p3 $
    wordBase128LEVar_choose 4 wordBase128LE_p4 $
    (\x -> liftFixedToBoundedR (wordBase128LE_p5 0## x))
  ) x0
{-# INLINE word32Base128LEVar_inline #-}

wordBase128LEVar_choose ::
  forall v w .
  (KnownNat v, KnownNat w) =>
  Int ->
  (Word# -> Word# -> FixedPrimR v) ->
  (Word# -> BoundedPrimR w) ->
  Word# -> BoundedPrimR (Max w v)
wordBase128LEVar_choose = \d f g x ->
  ifNat (W# x <= shiftL 1 (7 * d) - 1) (liftFixedToBoundedR (f 0## x)) (g x)
  -- We have observed GHC v8.6.5 jumping on the 'False' branch
  -- and falling through on the 'True' branch.  We set up our
  -- condition to favor lower numeric values.
{-# INLINE wordBase128LEVar_choose #-}

wordBase128LE_msb ::
  forall n .
  KnownNat n =>
  (Word# -> Word# -> FixedPrimR n) ->
  Word# -> Word# -> FixedPrimR (n + 1)
wordBase128LE_msb = \p m x ->
    p 0x80## x >+< word8 (W8# (or# m (uncheckedShiftRL# x s)))
  where
    !(I# s) = 7 * fromInteger (natVal' (proxy# :: Proxy# n))
{-# INLINE wordBase128LE_msb #-}

wordBase128LE_p1 :: Word# -> Word# -> FixedPrimR 1
wordBase128LE_p1 = \m x -> word8 (W8# (or# m x))
{-# INLINE wordBase128LE_p1 #-}

wordBase128LE_p2 :: Word# -> Word# -> FixedPrimR 2
wordBase128LE_p2 = wordBase128LE_msb wordBase128LE_p1
{-# INLINE wordBase128LE_p2 #-}

wordBase128LE_p3 :: Word# -> Word# -> FixedPrimR 3
wordBase128LE_p3 = wordBase128LE_msb wordBase128LE_p2
{-# INLINE wordBase128LE_p3 #-}

wordBase128LE_p4 :: Word# -> Word# -> FixedPrimR 4
wordBase128LE_p4 = wordBase128LE_msb wordBase128LE_p3
{-# INLINE wordBase128LE_p4 #-}

wordBase128LE_p5 :: Word# -> Word# -> FixedPrimR 5
wordBase128LE_p5 = wordBase128LE_msb wordBase128LE_p4
{-# INLINE wordBase128LE_p5 #-}

-- | Writes 1 or 2 base-128 digits in little-endian order;
-- in the 2-digit case the high bit of the containing byte of
-- the low digit is set, and the other byte has a clear high bit.
--
-- WARNING: The argument is ASSUMED to be in [0 .. 2^14 - 1].
word14Base128LEVar :: Word# -> BoundedPrimR 2
word14Base128LEVar = \x0 ->
  ( wordBase128LEVar_choose 1 wordBase128LE_p1 $
    (\x -> liftFixedToBoundedR (wordBase128LE_p2 0## x))
  ) x0
{-# INLINE word14Base128LEVar #-}

-- | Writes four base-128 digits, one per byte, with
-- the high bit of each byte set, in little-endian order.
--
-- There is no requirement that the argument be @< 2^28@.
word28Base128LE :: Word# -> FixedPrimR 4
word28Base128LE = wordBase128LE_p4 0x80##
{-# INLINE word28Base128LE #-}

-- | The bounded primitive implementing
-- `Proto3.Wire.Reverse.word64Base128LEVar`.
word64Base128LEVar :: Word64 -> BoundedPrimR 10
word64Base128LEVar = \(W64# x) ->
    ifNat (W64# x <= fromIntegral (maxBound :: Word32))
          (word32Base128LEVar (fromIntegral (W64# x)))
          (word64Base128LEVar_big x)
{-# INLINE word64Base128LEVar #-}

-- | Like 'word64Base128LEVar' but inlined, possibly bloating your code.  On
-- the other hand, inlining an application to a constant may shrink your code.
word64Base128LEVar_inline :: Word64 -> BoundedPrimR 10
word64Base128LEVar_inline = \(W64# x) ->
    ifNat (W64# x <= fromIntegral (maxBound :: Word32))
          (word32Base128LEVar (fromIntegral (W64# x)))
          (inline (word64Base128LEVar_big x))
{-# INLINE word64Base128LEVar_inline #-}

-- | The input must be at least 2^32.
word64Base128LEVar_big :: WORD64 -> BoundedPrimR 10
word64Base128LEVar_big x = ifNat (W64# x <= shiftL 1 60 - 1) p60 p64
  where
    p60 = liftFixedToBoundedR (word28Base128LE x32) >+<
          word32Base128LEVar (W32# (shR 28))

    p64 = liftFixedToBoundedR (word28Base128LE x32) >+<
          liftFixedToBoundedR (word28Base128LE (shR 28)) >+<
          word14Base128LEVar (shR 56)

    x32 = case fromIntegral (W64# x) of W32# y -> y

    shR s = case fromIntegral (shiftR (W64# x) s) of W32# y -> y
{-# NOINLINE word64Base128LEVar_big #-}

-- | The analog of `Proto3.Wire.Reverse.vectorBuildR` for when fixed-width
-- primitives encode the elements of the vector.  In this special case we
-- can predict the overall length.
vectorFixedPrimR ::
  forall w v a .
  (KnownNat w, Data.Vector.Generic.Vector v a) =>
  (a -> FixedPrimR w) ->
  v a ->
  BuildR
vectorFixedPrimR f = etaBuildR $ \v ->
    let op acc x = acc <> unsafeBoundedPrimR (liftFixedToBoundedR (f x))
    in ensure (w * Data.Vector.Generic.length v) (foldlRVector op mempty v)
  where
    w = fromInteger (natVal' (proxy# :: Proxy# w))
{-# INLINE vectorFixedPrimR #-}
