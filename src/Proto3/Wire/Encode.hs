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
-- one can use the 'Monoid' instance for 'Builder' to encode multiple
-- fields.
--
-- One should be careful to make sure that 'FieldNumber's appear in
-- increasing order.
--
-- In protocol buffers version 3, all fields are optional. To omit a value
-- for a field, simply do not append it to the 'Builder'. One can
-- create functions for wrapping optional fields with a 'Maybe' type.
--
-- Similarly, repeated fields can be encoded by concatenating several values
-- with the same 'FieldNumber'.
--
-- For example:
--
-- > strings :: Foldable f => FieldNumber -> f String -> Builder
-- > strings = foldMap . string
-- >
-- > 1 `strings` Just "some string" <>
-- > 2 `strings` [ "foo", "bar", "baz" ]

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Proto3.Wire.Encode
    ( -- * Standard Integers
      int32
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
    , packedFixed32
    , packedFixed64
    , packedFloats
    , packedDoubles
      -- * Reexports
    , Builder
    , WB.builderLength
    , WB.rawBuilder
    , WB.toLazyByteString
    ) where

import           Data.Bits                     ( (.&.), (.|.), shiftL, shiftR, xor )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Int                      ( Int32, Int64 )
import           Data.Monoid                   ( (<>) )
import qualified Data.Text.Encoding            as Text.Encoding
import qualified Data.Text.Lazy                as Text.Lazy
import qualified Data.Text.Lazy.Encoding       as Text.Lazy.Encoding
import           Data.Word                     ( Word8, Word32, Word64 )
import           Proto3.Wire.Builder           ( Builder )
import qualified Proto3.Wire.Builder           as WB
import           Proto3.Wire.Types

-- | $setup
--
-- >>> :set -XOverloadedStrings

base128Varint :: Word64 -> Builder
base128Varint i
    | i .&. 0x7f == i = WB.word8 (fromIntegral i)
    | otherwise = WB.word8 (0x80 .|. (fromIntegral i .&. 0x7f)) <>
          base128Varint (i `shiftR` 7)

wireType :: WireType -> Word8
wireType Varint = 0
wireType Fixed32 = 5
wireType Fixed64 = 1
wireType LengthDelimited = 2

fieldHeader :: FieldNumber -> WireType -> Builder
fieldHeader num wt = base128Varint ((getFieldNumber num `shiftL` 3) .|.
                                        fromIntegral (wireType wt))

-- | Encode a 32-bit "standard" integer
--
-- For example:
--
-- >>> 1 `int32` 42
-- Proto3.Wire.Builder.lazyByteString "\b*"
int32 :: FieldNumber -> Int32 -> Builder
int32 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 64-bit "standard" integer
--
-- For example:
--
-- >>> 1 `int64` (-42)
-- Proto3.Wire.Builder.lazyByteString "\b\214\255\255\255\255\255\255\255\255\SOH"
int64 :: FieldNumber -> Int64 -> Builder
int64 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 32-bit unsigned integer
--
-- For example:
--
-- >>> 1 `uint32` 42
-- Proto3.Wire.Builder.lazyByteString "\b*"
uint32 :: FieldNumber -> Word32 -> Builder
uint32 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 64-bit unsigned integer
--
-- For example:
--
-- >>> 1 `uint64` 42
-- Proto3.Wire.Builder.lazyByteString "\b*"
uint64 :: FieldNumber -> Word64 -> Builder
uint64 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 32-bit signed integer
--
-- For example:
--
-- >>> 1 `sint32` (-42)
-- Proto3.Wire.Builder.lazyByteString "\bS"
sint32 :: FieldNumber -> Int32 -> Builder
sint32 num i = int32 num ((i `shiftL` 1) `xor` (i `shiftR` 31))

-- | Encode a 64-bit signed integer
--
-- For example:
--
-- >>> 1 `sint64` (-42)
-- Proto3.Wire.Builder.lazyByteString "\bS"
sint64 :: FieldNumber -> Int64 -> Builder
sint64 num i = int64 num ((i `shiftL` 1) `xor` (i `shiftR` 63))

-- | Encode a fixed-width 32-bit integer
--
-- For example:
--
-- >>> 1 `fixed32` 42
-- Proto3.Wire.Builder.lazyByteString "\r*\NUL\NUL\NUL"
fixed32 :: FieldNumber -> Word32 -> Builder
fixed32 num i = fieldHeader num Fixed32 <> WB.word32LE i

-- | Encode a fixed-width 64-bit integer
--
-- For example:
--
-- >>> 1 `fixed64` 42
-- Proto3.Wire.Builder.lazyByteString "\t*\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
fixed64 :: FieldNumber -> Word64 -> Builder
fixed64 num i = fieldHeader num Fixed64 <> WB.word64LE i

-- | Encode a fixed-width signed 32-bit integer
--
-- For example:
--
-- > 1 `sfixed32` (-42)
sfixed32 :: FieldNumber -> Int32 -> Builder
sfixed32 num i = fieldHeader num Fixed32 <> WB.int32LE i

-- | Encode a fixed-width signed 64-bit integer
--
-- For example:
--
-- >>> 1 `sfixed64` (-42)
-- Proto3.Wire.Builder.lazyByteString "\t\214\255\255\255\255\255\255\255"
sfixed64 :: FieldNumber -> Int64 -> Builder
sfixed64 num i = fieldHeader num Fixed64 <> WB.int64LE i

-- | Encode a floating point number
--
-- For example:
--
-- >>> 1 `float` 3.14
-- Proto3.Wire.Builder.lazyByteString "\r\195\245H@"
float :: FieldNumber -> Float -> Builder
float num f = fieldHeader num Fixed32 <> WB.floatLE f

-- | Encode a double-precision number
--
-- For example:
--
-- >>> 1 `double` 3.14
-- Proto3.Wire.Builder.lazyByteString "\t\US\133\235Q\184\RS\t@"
double :: FieldNumber -> Double -> Builder
double num d = fieldHeader num Fixed64 <> WB.doubleLE d

-- | Encode a value with an enumerable type.
--
-- It can be useful to derive an 'Enum' instance for a type in order to
-- emulate enums appearing in .proto files.
--
-- For example:
--
-- >>> data Shape = Circle | Square | Triangle deriving (Enum)
-- >>> 1 `enum` True <> 2 `enum` Circle
-- Proto3.Wire.Builder.lazyByteString "\b\SOH\DLE\NUL"
enum :: Enum e => FieldNumber -> e -> Builder
enum num e = fieldHeader num Varint <> base128Varint (fromIntegral (fromEnum e))

-- | Encode a sequence of octets as a field of type 'bytes'.
--
-- >>> 1 `bytes` (Proto3.Wire.Builder.stringUtf8 "testing")
-- Proto3.Wire.Builder.lazyByteString "\n\atesting"
bytes :: FieldNumber -> Builder -> Builder
bytes = embedded

-- | Encode a UTF-8 string.
--
-- For example:
--
-- >>> 1 `string` "testing"
-- Proto3.Wire.Builder.lazyByteString "\n\atesting"
string :: FieldNumber -> String -> Builder
string num = embedded num . WB.stringUtf8

-- | Encode lazy `Text` as UTF-8
--
-- For example:
--
-- >>> 1 `text` "testing"
-- Proto3.Wire.Builder.lazyByteString "\n\atesting"
text :: FieldNumber -> Text.Lazy.Text -> Builder
text num txt =
    embedded num (WB.unsafeMakeBuilder len (Text.Lazy.Encoding.encodeUtf8Builder txt))
  where
    -- It would be nice to avoid actually allocating encoded chunks,
    -- but we leave that enhancement for a future time.
    len = Text.Lazy.foldrChunks op 0 txt
    op chnk acc = fromIntegral (B.length (Text.Encoding.encodeUtf8 chnk)) + acc
{-# INLINABLE text #-}
  -- INLINABLE so that if the input is constant, the compiler
  -- has the opportunity to express its length as a CAF.

-- | Encode a collection of bytes in the form of a strict 'B.ByteString'.
--
-- For example:
--
-- >>> 1 `byteString` "testing"
-- Proto3.Wire.Builder.lazyByteString "\n\atesting"
byteString :: FieldNumber -> B.ByteString -> Builder
byteString num bs = embedded num (WB.byteString bs)

-- | Encode a lazy bytestring.
--
-- For example:
--
-- >>> 1 `lazyByteString` "testing"
-- Proto3.Wire.Builder.lazyByteString "\n\atesting"
lazyByteString :: FieldNumber -> BL.ByteString -> Builder
lazyByteString num bl = embedded num (WB.lazyByteString bl)

-- | Encode varints in the space-efficient packed format.
--
-- >>> 1 `packedVarints` [1, 2, 3]
-- Proto3.Wire.Builder.lazyByteString "\n\ETX\SOH\STX\ETX"
packedVarints :: Foldable f => FieldNumber -> f Word64 -> Builder
packedVarints num = embedded num . foldMap base128Varint

-- | Encode fixed-width Word32s in the space-efficient packed format.
--
-- >>> 1 `packedFixed32` [1, 2, 3]
-- Proto3.Wire.Builder.lazyByteString "\n\f\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL\ETX\NUL\NUL\NUL"
packedFixed32 :: Foldable f => FieldNumber -> f Word32 -> Builder
packedFixed32 num = embedded num . foldMap WB.word32LE

-- | Encode fixed-width Word64s in the space-efficient packed format.
--
-- >>> 1 `packedFixed64` [1, 2, 3]
-- Proto3.Wire.Builder.lazyByteString "\n\CAN\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
packedFixed64 :: Foldable f => FieldNumber -> f Word64 -> Builder
packedFixed64 num = embedded num . foldMap WB.word64LE

-- | Encode floats in the space-efficient packed format.
--
-- >>> 1 `packedFloats` [1, 2, 3]
-- Proto3.Wire.Builder.lazyByteString "\n\f\NUL\NUL\128?\NUL\NUL\NUL@\NUL\NUL@@"
packedFloats :: Foldable f => FieldNumber -> f Float -> Builder
packedFloats num = embedded num . foldMap WB.floatLE

-- | Encode doubles in the space-efficient packed format.
--
-- >>> 1 `packedDoubles` [1, 2, 3]
-- Proto3.Wire.Builder.lazyByteString "\n\CAN\NUL\NUL\NUL\NUL\NUL\NUL\240?\NUL\NUL\NUL\NUL\NUL\NUL\NUL@\NUL\NUL\NUL\NUL\NUL\NUL\b@"
packedDoubles :: Foldable f => FieldNumber -> f Double -> Builder
packedDoubles num = embedded num . foldMap WB.doubleLE

-- | Encode an embedded message.
--
-- The message is represented as a 'Builder', so it is possible to chain
-- encoding functions.
--
-- For example:
--
-- >>> 1 `embedded` (1 `string` "this message" <> 2 `string` " is embedded")
-- Proto3.Wire.Builder.lazyByteString "\n\FS\n\fthis message\DC2\f is embedded"
embedded :: FieldNumber -> Builder -> Builder
embedded num bb = fieldHeader num LengthDelimited <>
    base128Varint (fromIntegral (WB.builderLength bb)) <>
    bb
