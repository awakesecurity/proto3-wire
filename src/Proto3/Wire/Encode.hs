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
-- > fieldNumber 1 `strings` Just "some string" <>
-- > fieldNumber 2 `strings` [ "foo", "bar", "baz" ]

{-# LANGUAGE BangPatterns #-}
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
    , builderLength
    , toLazyByteString
    ) where

import           Data.Bits                     ( (.&.), (.|.), shiftL, shiftR, xor )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Lazy          as BL
import           Data.Char                     ( ord )
import           Data.Int                      ( Int32, Int64 )
import           Data.Monoid                   ( Sum(..), (<>) )
import qualified Data.Text.Encoding            as Text.Encoding
import qualified Data.Text.Lazy                as Text.Lazy
import qualified Data.Text.Lazy.Encoding       as Text.Lazy.Encoding
import           Data.Word                     ( Word32, Word64, Word8 )
import           Proto3.Wire.Types

-- | Like 'BB.Builder', but memoizes the resulting length so
-- that we can efficiently encode nested embedded messages.
newtype Builder = Builder { unBuilder :: (Sum Word, BB.Builder) }
  deriving Monoid

builderLength :: Builder -> Word
builderLength = getSum . fst . unBuilder

toLazyByteString :: Builder -> BL.ByteString
toLazyByteString (Builder (Sum len, bb)) =
    BB.toLazyByteStringWith strat BL.empty bb
  where
    -- If the supplied length is accurate then we will perform
    -- just one allocation.  Any inaccuracy would indicate
    -- a bug in one of the primitives that produces a 'Builder'.
    maxFirstChunk = 134217728  -- 128MiB
    firstChunk = fromIntegral (min maxFirstChunk len)
    strat = BB.safeStrategy firstChunk BB.defaultChunkSize
{-# NOINLINE toLazyByteString #-}

word8 :: Word8 -> Builder
word8 w = Builder (Sum 1, BB.word8 w)

word32LE :: Word32 -> Builder
word32LE w = Builder (Sum 4, BB.word32LE w)

int32LE :: Int32 -> Builder
int32LE w = Builder (Sum 4, BB.int32LE w)

floatLE :: Float -> Builder
floatLE f = Builder (Sum 4, BB.floatLE f)

word64LE :: Word64 -> Builder
word64LE w = Builder (Sum 8, BB.word64LE w)

int64LE :: Int64 -> Builder
int64LE w = Builder (Sum 8, BB.int64LE w)

doubleLE :: Double -> Builder
doubleLE f = Builder (Sum 8, BB.doubleLE f)

stringUtf8 :: String -> Builder
stringUtf8 s = Builder (Sum (len 0 s), BB.stringUtf8 s)
  where
    len !n []      = n
    len !n (h : t) = case ord h of
      c | c <= 0x7F   -> len (n + 1) t
        | c <= 0x07FF -> len (n + 2) t
        | c <= 0xFFFF -> len (n + 3) t
        | otherwise   -> len (n + 4) t

base128Varint :: Word64 -> Builder
base128Varint i
    | i .&. 0x7f == i = word8 (fromIntegral i)
    | otherwise = word8 (0x80 .|. (fromIntegral i .&. 0x7f)) <>
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
-- > fieldNumber 1 `int32` 42
int32 :: FieldNumber -> Int32 -> Builder
int32 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 64-bit "standard" integer
--
-- For example:
--
-- > fieldNumber 1 `int64` negate 42
int64 :: FieldNumber -> Int64 -> Builder
int64 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 32-bit unsigned integer
--
-- For example:
--
-- > fieldNumber 1 `uint32` 42
uint32 :: FieldNumber -> Word32 -> Builder
uint32 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 64-bit unsigned integer
--
-- For example:
--
-- > fieldNumber 1 `uint64` 42
uint64 :: FieldNumber -> Word64 -> Builder
uint64 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 32-bit signed integer
--
-- For example:
--
-- > fieldNumber 1 `sint32` negate 42
sint32 :: FieldNumber -> Int32 -> Builder
sint32 num i = int32 num ((i `shiftL` 1) `xor` (i `shiftR` 31))

-- | Encode a 64-bit signed integer
--
-- For example:
--
-- > fieldNumber 1 `sint64` negate 42
sint64 :: FieldNumber -> Int64 -> Builder
sint64 num i = int64 num ((i `shiftL` 1) `xor` (i `shiftR` 63))

-- | Encode a fixed-width 32-bit integer
--
-- For example:
--
-- > fieldNumber 1 `fixed32` 42
fixed32 :: FieldNumber -> Word32 -> Builder
fixed32 num i = fieldHeader num Fixed32 <> word32LE i

-- | Encode a fixed-width 64-bit integer
--
-- For example:
--
-- > fieldNumber 1 `fixed64` 42
fixed64 :: FieldNumber -> Word64 -> Builder
fixed64 num i = fieldHeader num Fixed64 <> word64LE i

-- | Encode a fixed-width signed 32-bit integer
--
-- For example:
--
-- > fieldNumber 1 `sfixed32` negate 42
sfixed32 :: FieldNumber -> Int32 -> Builder
sfixed32 num i = fieldHeader num Fixed32 <> int32LE i

-- | Encode a fixed-width signed 64-bit integer
--
-- For example:
--
-- > fieldNumber 1 `sfixed64` negate 42
sfixed64 :: FieldNumber -> Int64 -> Builder
sfixed64 num i = fieldHeader num Fixed64 <> int64LE i

-- | Encode a floating point number
--
-- For example:
--
-- > fieldNumber 1 `float` 3.14
float :: FieldNumber -> Float -> Builder
float num f = fieldHeader num Fixed32 <> floatLE f

-- | Encode a double-precision number
--
-- For example:
--
-- > fieldNumber 1 `double` 3.14
double :: FieldNumber -> Double -> Builder
double num d = fieldHeader num Fixed64 <> doubleLE d

-- | Encode a value with an enumerable type.
--
-- It can be useful to derive an 'Enum' instance for a type in order to
-- emulate enums appearing in .proto files.
--
-- For example:
--
-- > data Shape = Circle | Square | Triangle
-- >   deriving (Show, Eq, Ord, Enum)
-- >
-- > fieldNumber 1 `enum` True <>
-- > fieldNumber 2 `enum` Circle
enum :: Enum e => FieldNumber -> e -> Builder
enum num e = fieldHeader num Varint <> base128Varint (fromIntegral (fromEnum e))

-- | Encode a UTF-8 string.
--
-- For example:
--
-- > fieldNumber 1 `string` "testing"
string :: FieldNumber -> String -> Builder
string num = embedded num . stringUtf8

-- | Encode lazy `Text` as UTF-8
--
-- For example:
--
-- > fieldNumber 1 `text` "testing"
text :: FieldNumber -> Text.Lazy.Text -> Builder
text num txt =
    embedded num (Builder (Sum len, Text.Lazy.Encoding.encodeUtf8Builder txt))
  where
    -- It would be nice to avoid actually allocating encoded chunks,
    -- but we leave that enhancement for a future time.
    len = Text.Lazy.foldrChunks op 0 txt
    op chnk acc = fromIntegral (B.length (Text.Encoding.encodeUtf8 chnk)) + acc

-- | Encode a collection of bytes in the form of a strict 'B.ByteString'.
--
-- For example:
--
-- > fieldNumber 1 `byteString` fromString "some bytes"
byteString :: FieldNumber -> B.ByteString -> Builder
byteString num bs = embedded num bldr
  where
    bldr = (Builder (Sum (fromIntegral (B.length bs)), BB.byteString bs))

-- | Encode a lazy bytestring.
--
-- For example:
--
-- > fieldNumber 1 `lazyByteString` fromString "some bytes"
lazyByteString :: FieldNumber -> BL.ByteString -> Builder
lazyByteString num bl = embedded num bldr
  where
    bldr = (Builder (Sum (fromIntegral (BL.length bl)), BB.lazyByteString bl))

-- | Encode varints in the space-efficient packed format.
packedVarints :: Foldable f => FieldNumber -> f Word64 -> Builder
packedVarints num = embedded num . foldMap base128Varint

-- | Encode fixed-width Word32s in the space-efficient packed format.
packedFixed32 :: Foldable f => FieldNumber -> f Word32 -> Builder
packedFixed32 num = embedded num . foldMap word32LE

-- | Encode fixed-width Word64s in the space-efficient packed format.
packedFixed64 :: Foldable f => FieldNumber -> f Word64 -> Builder
packedFixed64 num = embedded num . foldMap word64LE

-- | Encode floats in the space-efficient packed format.
packedFloats :: Foldable f => FieldNumber -> f Float -> Builder
packedFloats num = embedded num . foldMap floatLE

-- | Encode doubles in the space-efficient packed format.
packedDoubles :: Foldable f => FieldNumber -> f Double -> Builder
packedDoubles num = embedded num . foldMap doubleLE

-- | Encode an embedded message.
--
-- The message is represented as a 'BB.Builder', so it is possible to chain
-- encoding functions.
--
-- For example:
--
-- > embedded (fieldNumber 1) $
-- >   fieldNumber (fieldNumber 1) `string` "this message" <>
-- >   fieldNumber (fieldNumber 2) `string` " is embedded"
embedded :: FieldNumber -> Builder -> Builder
embedded num bb = fieldHeader num LengthDelimited <>
    base128Varint (fromIntegral (builderLength bb)) <>
    bb
