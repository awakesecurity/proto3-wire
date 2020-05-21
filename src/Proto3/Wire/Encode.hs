{-
  Copyright 2016 Awake Networks

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

-- | Low level functions for writing the protobufs wire format.
--
-- Because protobuf messages are encoded as a collection of fields,
-- one can use the 'Monoid' instance for 'MessageBuilder' to encode multiple
-- fields.
--
-- One should be careful to make sure that 'FieldNumber's appear in
-- increasing order.
--
-- In protocol buffers version 3, all fields are optional. To omit a value
-- for a field, simply do not append it to the 'MessageBuilder'. One can
-- create functions for wrapping optional fields with a 'Maybe' type.
--
-- Similarly, repeated fields can be encoded by concatenating several values
-- with the same 'FieldNumber'.
--
-- For example:
--
-- > strings :: Foldable f => FieldNumber -> f String -> MessageBuilder
-- > strings = foldMap . string
-- >
-- > 1 `strings` Just "some string" <>
-- > 2 `strings` [ "foo", "bar", "baz" ]

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Proto3.Wire.Encode
    ( -- * `MessageBuilder` type
      MessageBuilder
    , vectorMessageBuilder
    , messageLength
    , toLazyByteString
    , unsafeFromLazyByteString

      -- * Standard Integers
    , int32
    , int64
      -- * Unsigned Integers
    , uint32
    , uint64
      -- * Signed Integers
    , sint32
    , sint64
      -- * Non-varint Numbers
    , fixed32
    , fixed64
    , sfixed32
    , sfixed64
    , float
    , double
    , enum
    , bool
      -- * Strings
    , bytes
    , string
    , text
    , byteString
    , lazyByteString
      -- * Embedded Messages
    , embedded
      -- * Packed repeated fields
    , packedVarints
    , packedVarintsV
    , packedFixed32
    , packedFixed32V
    , packedFixed64
    , packedFixed64V
    , packedFloats
    , packedFloatsV
    , packedDoubles
    , packedDoublesV
    ) where

import           Data.Bits                     ( (.|.), shiftL, shiftR, xor )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Coerce                   ( coerce )
import           Data.Int                      ( Int32, Int64 )
import qualified Data.Text.Lazy                as Text.Lazy
import qualified Data.Vector.Unboxed
import           Data.Vector.Generic           ( Vector )
import           Data.Word                     ( Word8, Word32, Word64 )
import           GHC.TypeLits                  ( KnownNat )
import qualified Proto3.Wire.Reverse           as RB
import qualified Proto3.Wire.Reverse.Prim      as Prim
import           Proto3.Wire.Reverse.Width     ( ChooseNat, MonoidNat,
                                                 SemigroupNat(..) )
import           Proto3.Wire.Class
import           Proto3.Wire.Types

-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedLists
-- >>> :module Proto3.Wire.Encode Proto3.Wire.Class Data.Word

-- | A `MessageBuilder` represents a serialized protobuf message
--
-- Use the utilities provided by this module to create `MessageBuilder`s
--
-- You can concatenate two messages using the `Monoid` instance for
-- `MessageBuilder`
--
-- Use `toLazyByteString` when you're done assembling the `MessageBuilder`
newtype MessageBuilder = MessageBuilder { unMessageBuilder :: RB.BuildR }
  deriving (Monoid, Semigroup)

instance Show MessageBuilder where
  showsPrec prec builder =
      showParen (prec > 10)
        (showString "Proto3.Wire.Encode.unsafeFromLazyByteString " . shows bytes')
    where
      bytes' = toLazyByteString builder

etaMessageBuilder :: forall a . (a -> MessageBuilder) -> a -> MessageBuilder
etaMessageBuilder = coerce (RB.etaBuildR @a)

vectorMessageBuilder ::
  forall v a . Vector v a => (a -> MessageBuilder) -> v a -> MessageBuilder
vectorMessageBuilder = coerce (RB.vectorBuildR @v @a)

-- | O(n): Retrieve the length of a message, in bytes.
messageLength :: MessageBuilder -> Word
messageLength = fromIntegral . fst . RB.runBuildR . unMessageBuilder

-- | Convert a message to a lazy `BL.ByteString`
toLazyByteString :: MessageBuilder -> BL.ByteString
toLazyByteString = RB.toLazyByteString . unMessageBuilder

-- | This lets you cast an arbitrary `ByteString` to a `MessageBuilder`, whether
-- or not the `ByteString` corresponds to a valid serialized protobuf message
--
-- Do not use this function unless you know what you're doing because it lets
-- you assemble malformed protobuf `MessageBuilder`s
unsafeFromLazyByteString :: BL.ByteString -> MessageBuilder
unsafeFromLazyByteString bytes' =
    MessageBuilder { unMessageBuilder = RB.lazyByteString bytes' }

newtype MessageBoundedPrim w
  = MessageBoundedPrim { unMessageBoundedPrim :: Prim.BoundedPrimR w }
  deriving (ChooseNat, MonoidNat, SemigroupNat)

primBounded :: KnownNat w => MessageBoundedPrim w -> MessageBuilder
primBounded (MessageBoundedPrim p) = MessageBuilder (Prim.primBoundedR p)
{-# INLINE primBounded #-}

base128Varint32 :: Word32 -> MessageBoundedPrim 5
base128Varint32 = MessageBoundedPrim . Prim.word32Base128LEVar
{-# INLINE base128Varint32 #-}

base128Varint64 :: Word64 -> MessageBoundedPrim 10
base128Varint64 = MessageBoundedPrim . Prim.word64Base128LEVar
{-# INLINE base128Varint64 #-}

base128Varint64_inline :: Word64 -> MessageBoundedPrim 10
base128Varint64_inline = MessageBoundedPrim . Prim.word64Base128LEVar_inline
{-# INLINE base128Varint64_inline #-}

wireType :: WireType -> Word8
wireType Varint = 0
wireType Fixed32 = 5
wireType Fixed64 = 1
wireType LengthDelimited = 2

fieldHeader :: FieldNumber -> WireType -> MessageBoundedPrim 10
fieldHeader = \num wt -> base128Varint64_inline
    ((getFieldNumber num `shiftL` 3) .|. fromIntegral (wireType wt))
{-# INLINE fieldHeader #-}

-- | Encode a 32-bit "standard" integer
--
-- For example:
--
-- >>> 1 `int32` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b*"
-- >>> 1 `int64` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\214\255\255\255\255\255\255\255\255\SOH"
--
-- NOTE: Protobuf encoding converts an @int32@ to a 64-bit unsigned value
-- before encoding it, not a 32-bit value (which would be more efficient).
--
-- To quote the specification: "If you use int32 or int64 as the type for
-- a negative number, the resulting varint is always ten bytes long..."
-- <https://developers.google.com/protocol-buffers/docs/encoding#varints>
int32 :: FieldNumber -> Int32 -> MessageBuilder
int32 = \num i -> primBounded $
    fieldHeader num Varint >+< base128Varint64 (fromIntegral i)
{-# INLINE int32 #-}

-- | Encode a 64-bit "standard" integer
--
-- For example:
--
-- >>> 1 `int32` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b*"
-- >>> 1 `int64` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\214\255\255\255\255\255\255\255\255\SOH"
int64 :: FieldNumber -> Int64 -> MessageBuilder
int64 = \num i -> primBounded $
    fieldHeader num Varint >+< base128Varint64 (fromIntegral i)
{-# INLINE int64 #-}

-- | Encode a 32-bit unsigned integer
--
-- For example:
--
-- >>> 1 `uint32` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b*"
uint32 :: FieldNumber -> Word32 -> MessageBuilder
uint32 = \num i -> primBounded $
    fieldHeader num Varint >+< base128Varint32 i
{-# INLINE uint32 #-}

-- | Encode a 64-bit unsigned integer
--
-- For example:
--
-- >>> 1 `uint64` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b*"
uint64 :: FieldNumber -> Word64 -> MessageBuilder
uint64 = \num i -> primBounded $
    fieldHeader num Varint >+< base128Varint64 i
{-# INLINE uint64 #-}

-- | Encode a 32-bit signed integer
--
-- For example:
--
-- >>> 1 `sint32` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\bS"
-- >>> 1 `sint32` maxBound
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\254\255\255\255\SI"
-- >>> 1 `sint32` minBound
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\255\255\255\255\SI"
sint32 :: FieldNumber -> Int32 -> MessageBuilder
sint32 = \num i ->
  uint32 num (fromIntegral ((i `shiftL` 1) `xor` (i `shiftR` 31)))
{-# INLINE sint32 #-}

-- | Encode a 64-bit signed integer
--
-- For example:
--
-- >>> 1 `sint64` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\bS"
-- >>> 1 `sint64` maxBound
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\254\255\255\255\255\255\255\255\255\SOH"
-- >>> 1 `sint64` minBound
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\255\255\255\255\255\255\255\255\255\SOH"
sint64 :: FieldNumber -> Int64 -> MessageBuilder
sint64 = \num i ->
  uint64 num (fromIntegral ((i `shiftL` 1) `xor` (i `shiftR` 63)))
{-# INLINE sint64 #-}

-- | Encode a fixed-width 32-bit integer
--
-- For example:
--
-- >>> 1 `fixed32` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\r*\NUL\NUL\NUL"
fixed32 :: FieldNumber -> Word32 -> MessageBuilder
fixed32 = \num i -> primBounded $
    fieldHeader num Fixed32 >+<
    MessageBoundedPrim (Prim.liftFixedToBoundedR (Prim.word32LE i))
{-# INLINE fixed32 #-}

-- | Encode a fixed-width 64-bit integer
--
-- For example:
--
-- >>> 1 `fixed64` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\t*\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
fixed64 :: FieldNumber -> Word64 -> MessageBuilder
fixed64 = \num i -> primBounded $
    fieldHeader num Fixed64 >+<
    MessageBoundedPrim (Prim.liftFixedToBoundedR (Prim.word64LE i))
{-# INLINE fixed64 #-}

-- | Encode a fixed-width signed 32-bit integer
--
-- For example:
--
-- > 1 `sfixed32` (-42)
sfixed32 :: FieldNumber -> Int32 -> MessageBuilder
sfixed32 = \num i -> primBounded $
    fieldHeader num Fixed32 >+<
    MessageBoundedPrim (Prim.liftFixedToBoundedR (Prim.int32LE i))
{-# INLINE sfixed32 #-}

-- | Encode a fixed-width signed 64-bit integer
--
-- For example:
--
-- >>> 1 `sfixed64` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\t\214\255\255\255\255\255\255\255"
sfixed64 :: FieldNumber -> Int64 -> MessageBuilder
sfixed64 = \num i -> primBounded $
    fieldHeader num Fixed64 >+<
    MessageBoundedPrim (Prim.liftFixedToBoundedR (Prim.int64LE i))
{-# INLINE sfixed64 #-}

-- | Encode a floating point number
--
-- For example:
--
-- >>> 1 `float` 3.14
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\r\195\245H@"
float :: FieldNumber -> Float -> MessageBuilder
float = \num f -> primBounded $
    fieldHeader num Fixed32 >+<
    MessageBoundedPrim (Prim.liftFixedToBoundedR (Prim.floatLE f))
{-# INLINE float #-}

-- | Encode a double-precision number
--
-- For example:
--
-- >>> 1 `double` 3.14
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\t\US\133\235Q\184\RS\t@"
double :: FieldNumber -> Double -> MessageBuilder
double = \num d -> primBounded $
    fieldHeader num Fixed64 >+<
    MessageBoundedPrim (Prim.liftFixedToBoundedR (Prim.doubleLE d))
{-# INLINE double #-}

-- | Encode a value with an enumerable type.
--
-- You should instantiate 'ProtoEnum' for a type in
-- order to emulate enums appearing in .proto files.
--
-- For example:
--
-- >>> :{
--     data Shape = Circle | Square | Triangle deriving (Bounded, Enum)
--     instance ProtoEnum Shape
--     data Gap = Gap0 | Gap3
--     instance ProtoEnum Gap where
--       toProtoEnumMay i = case i of
--         0 -> Just Gap0
--         3 -> Just Gap3
--         _ -> Nothing
--       fromProtoEnum g = case g of
--         Gap0 -> 0
--         Gap3 -> 3
-- :}
--
-- >>> 1 `enum` Triangle <> 2 `enum` Gap3
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\STX\DLE\ETX"
enum :: ProtoEnum e => FieldNumber -> e -> MessageBuilder
enum = \num e -> primBounded $
    fieldHeader num Varint >+<
    base128Varint32 (fromIntegral @Int32 @Word32 (fromProtoEnum e))
{-# INLINE enum #-}

-- | Encode a boolean value
--
-- For example:
--
-- >>> 1 `bool` True
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\SOH"
bool :: FieldNumber -> Bool -> MessageBuilder
bool = \num b -> primBounded $
    fieldHeader num Varint >+<
    MessageBoundedPrim
      (Prim.liftFixedToBoundedR (Prim.word8 (fromIntegral (fromEnum b))))
      -- Using word8 instead of a varint encoder shrinks the width bound.
{-# INLINE bool #-}

-- | Encode a sequence of octets as a field of type 'bytes'.
--
-- >>> 1 `bytes` (Proto3.Wire.Reverse.stringUtf8 "testing")
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
bytes :: FieldNumber -> RB.BuildR -> MessageBuilder
bytes num = embedded num . MessageBuilder
{-# INLINE bytes #-}

-- | Encode a UTF-8 string.
--
-- For example:
--
-- >>> 1 `string` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
string :: FieldNumber -> String -> MessageBuilder
string num = embedded num . MessageBuilder . RB.stringUtf8
{-# INLINE string #-}

-- | Encode lazy `Text` as UTF-8
--
-- For example:
--
-- >>> 1 `text` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
text :: FieldNumber -> Text.Lazy.Text -> MessageBuilder
text num = embedded num . MessageBuilder . RB.lazyTextUtf8
{-# INLINE text #-}

-- | Encode a collection of bytes in the form of a strict 'B.ByteString'.
--
-- For example:
--
-- >>> 1 `byteString` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
byteString :: FieldNumber -> B.ByteString -> MessageBuilder
byteString num = embedded num . MessageBuilder . RB.byteString
{-# INLINE byteString #-}

-- | Encode a lazy bytestring.
--
-- For example:
--
-- >>> 1 `lazyByteString` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
lazyByteString :: FieldNumber -> BL.ByteString -> MessageBuilder
lazyByteString num = embedded num . MessageBuilder . RB.lazyByteString
{-# INLINE lazyByteString #-}

-- | Encode varints in the space-efficient packed format.
-- But consider 'packedVarintsV', which may be faster.
--
-- The values to be encoded are specified by mapping the elements of a vector.
--
-- >>> packedVarints 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\ETX\SOH\STX\ETX"
packedVarints :: Foldable f => FieldNumber -> f Word64 -> MessageBuilder
packedVarints num =
    etaMessageBuilder (embedded num . foldMap (primBounded . base128Varint64))
{-# INLINE packedVarints #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedVarints num . fmap f
--
-- >>> packedVarintsV (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Word64)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\ETX\SOH\STX\ETX"
packedVarintsV ::
  Vector v a => (a -> Word64) -> FieldNumber -> v a -> MessageBuilder
packedVarintsV f num =
    embedded num . vectorMessageBuilder (primBounded . base128Varint64 . f)
{-# INLINE packedVarintsV #-}

-- | Encode fixed-width Word32s in the space-efficient packed format.
-- But consider 'packedFixed32V', which may be faster.
--
-- The values to be encoded are specified by mapping the elements of a vector.
--
-- >>> packedFixed32 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL\ETX\NUL\NUL\NUL"
packedFixed32 :: Foldable f => FieldNumber -> f Word32 -> MessageBuilder
packedFixed32 num =
    etaMessageBuilder (embedded num . foldMap (lf . Prim.word32LE))
  where
    lf = MessageBuilder . Prim.unsafeBoundedPrimR . Prim.liftFixedToBoundedR
{-# INLINE packedFixed32 #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedFixed32 num . fmap f
--
-- >>> packedFixed32V (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Word32)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL\ETX\NUL\NUL\NUL"
packedFixed32V ::
  Vector v a => (a -> Word32) -> FieldNumber -> v a -> MessageBuilder
packedFixed32V f num =
    embedded num . MessageBuilder . Prim.vectorFixedPrimR (Prim.word32LE . f)
{-# INLINE packedFixed32V #-}

-- | Encode fixed-width Word64s in the space-efficient packed format.
-- But consider 'packedFixed64V', which may be faster.
--
-- The values to be encoded are specified by mapping the elements of a vector.
--
-- >>> packedFixed64 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
packedFixed64 :: Foldable f => FieldNumber -> f Word64 -> MessageBuilder
packedFixed64 num =
    etaMessageBuilder (embedded num . foldMap (lf . Prim.word64LE))
  where
    lf = MessageBuilder . Prim.unsafeBoundedPrimR . Prim.liftFixedToBoundedR
{-# INLINE packedFixed64 #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedFixed64 num . fmap f
--
-- >>> packedFixed64V (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Word64)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
packedFixed64V ::
  Vector v a => (a -> Word64) -> FieldNumber -> v a -> MessageBuilder
packedFixed64V f num =
    embedded num . MessageBuilder . Prim.vectorFixedPrimR (Prim.word64LE . f)
{-# INLINE packedFixed64V #-}

-- | Encode floats in the space-efficient packed format.
-- But consider 'packedFloatsV', which may be faster.
--
-- >>> 1 `packedFloats` [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\NUL\NUL\128?\NUL\NUL\NUL@\NUL\NUL@@"
packedFloats :: Foldable f => FieldNumber -> f Float -> MessageBuilder
packedFloats num =
    etaMessageBuilder (embedded num . foldMap (lf . Prim.floatLE))
  where
    lf = MessageBuilder . Prim.unsafeBoundedPrimR . Prim.liftFixedToBoundedR
{-# INLINE packedFloats #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedFloats num . fmap f
--
-- >>> packedFloatsV (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Float)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\NUL\NUL\128?\NUL\NUL\NUL@\NUL\NUL@@"
packedFloatsV ::
  Vector v a => (a -> Float) -> FieldNumber -> v a -> MessageBuilder
packedFloatsV f num =
    embedded num . MessageBuilder . Prim.vectorFixedPrimR (Prim.floatLE . f)
{-# INLINE packedFloatsV #-}

-- | Encode doubles in the space-efficient packed format.
-- But consider 'packedDoublesV', which may be faster.
--
-- >>> 1 `packedDoubles` [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\NUL\NUL\NUL\NUL\NUL\NUL\240?\NUL\NUL\NUL\NUL\NUL\NUL\NUL@\NUL\NUL\NUL\NUL\NUL\NUL\b@"
packedDoubles :: Foldable f => FieldNumber -> f Double -> MessageBuilder
packedDoubles num =
    etaMessageBuilder (embedded num . foldMap (lf . Prim.doubleLE))
  where
    lf = MessageBuilder . Prim.unsafeBoundedPrimR . Prim.liftFixedToBoundedR
{-# INLINE packedDoubles #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedDoubles num . fmap f
--
-- >>> packedDoublesV (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Double)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\NUL\NUL\NUL\NUL\NUL\NUL\240?\NUL\NUL\NUL\NUL\NUL\NUL\NUL@\NUL\NUL\NUL\NUL\NUL\NUL\b@"
packedDoublesV ::
  Vector v a => (a -> Double) -> FieldNumber -> v a -> MessageBuilder
packedDoublesV f num =
    embedded num . MessageBuilder . Prim.vectorFixedPrimR (Prim.doubleLE . f)
{-# INLINE packedDoublesV #-}

-- | Encode an embedded message.
--
-- The message is represented as a 'MessageBuilder', so it is possible to chain
-- encoding functions.
--
-- For example:
--
-- >>> 1 `embedded` (1 `string` "this message" <> 2 `string` " is embedded")
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\FS\n\fthis message\DC2\f is embedded"
embedded :: FieldNumber -> MessageBuilder -> MessageBuilder
embedded = \num (MessageBuilder bb) ->
    MessageBuilder (RB.withLengthOf (Prim.primBoundedR . prefix num) bb)
  where
    prefix num len =
      unMessageBoundedPrim (fieldHeader num LengthDelimited) >+<
      Prim.wordBase128LEVar (fromIntegral @Int @Word len)
{-# INLINE embedded #-}
