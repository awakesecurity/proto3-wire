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

-- | Extends "Data.ByteString.Builder" by memoizing the resulting length.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Proto3.Wire.Builder
    ( Builder
    , builderLength
    , rawBuilder
    , unsafeMakeBuilder
    , toLazyByteString
    , hPutBuilder
    , byteString
    , lazyByteString
    , shortByteString
    , word8
    , word16BE
    , word16LE
    , word32BE
    , word32LE
    , word64BE
    , word64LE
    , int8
    , int16BE
    , int16LE
    , int32BE
    , int32LE
    , int64BE
    , int64LE
    , floatBE
    , floatLE
    , doubleBE
    , doubleLE
    , char7
    , string7
    , char8
    , string8
    , charUtf8
    , stringUtf8
    ) where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Short         as BS
import           Data.Char                     ( ord )
import           Data.Int                      ( Int8, Int16, Int32, Int64 )
import           Data.Monoid                   ( Sum(..) )
import           Data.Word                     ( Word8, Word16, Word32, Word64 )
import           System.IO                     ( Handle )

-- | Like 'BB.Builder', but memoizes the resulting length so
-- that we can efficiently encode nested embedded messages.
newtype Builder = Builder { unBuilder :: (Sum Word, BB.Builder) }
  deriving Monoid

builderLength :: Builder -> Word
builderLength = getSum . fst . unBuilder

rawBuilder :: Builder -> BB.Builder
rawBuilder = snd . unBuilder

-- | Use with caution--the caller is responsible for ensuring that
-- the given length figure accurately measures the given builder.
unsafeMakeBuilder :: Word -> BB.Builder -> Builder
unsafeMakeBuilder len bldr = Builder (Sum len, bldr)

toLazyByteString :: Builder -> BL.ByteString
toLazyByteString (Builder (Sum len, bb)) =
    BB.toLazyByteStringWith strat BL.empty bb
  where
    -- If the supplied length is accurate then we will perform just
    -- one allocation.  An inaccurate length would indicate a bug
    -- in one of the primitives that produces a 'Builder'.
    strat = BB.safeStrategy (fromIntegral len) BB.defaultChunkSize
{-# NOINLINE toLazyByteString #-}
  -- NOINLINE to avoid bloating caller; see docs for 'BB.toLazyByteStringWith'.

hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder handle = BB.hPutBuilder handle . snd . unBuilder

byteString :: B.ByteString -> Builder
byteString bs = Builder (Sum (fromIntegral (B.length bs)), BB.byteString bs)

-- | Warning: evaluating the length will generate the input chunks,
-- and they will remain allocated until you finish using the builder.
lazyByteString :: BL.ByteString -> Builder
lazyByteString bl =
  Builder (Sum (fromIntegral (BL.length bl)), BB.lazyByteString bl)

shortByteString :: BS.ShortByteString -> Builder
shortByteString bs =
  Builder (Sum (fromIntegral (BS.length bs)), BB.shortByteString bs)

word8 :: Word8 -> Builder
word8 w = Builder (Sum 1, BB.word8 w)

int8 :: Int8 -> Builder
int8 w = Builder (Sum 1, BB.int8 w)

word16BE :: Word16 -> Builder
word16BE w = Builder (Sum 2, BB.word16BE w)

word16LE :: Word16 -> Builder
word16LE w = Builder (Sum 2, BB.word16LE w)

int16BE :: Int16 -> Builder
int16BE w = Builder (Sum 2, BB.int16BE w)

int16LE :: Int16 -> Builder
int16LE w = Builder (Sum 2, BB.int16LE w)

word32BE :: Word32 -> Builder
word32BE w = Builder (Sum 4, BB.word32BE w)

word32LE :: Word32 -> Builder
word32LE w = Builder (Sum 4, BB.word32LE w)

int32BE :: Int32 -> Builder
int32BE w = Builder (Sum 4, BB.int32BE w)

int32LE :: Int32 -> Builder
int32LE w = Builder (Sum 4, BB.int32LE w)

floatBE :: Float -> Builder
floatBE f = Builder (Sum 4, BB.floatBE f)

floatLE :: Float -> Builder
floatLE f = Builder (Sum 4, BB.floatLE f)

word64BE :: Word64 -> Builder
word64BE w = Builder (Sum 8, BB.word64BE w)

word64LE :: Word64 -> Builder
word64LE w = Builder (Sum 8, BB.word64LE w)

int64BE :: Int64 -> Builder
int64BE w = Builder (Sum 8, BB.int64BE w)

int64LE :: Int64 -> Builder
int64LE w = Builder (Sum 8, BB.int64LE w)

doubleBE :: Double -> Builder
doubleBE f = Builder (Sum 8, BB.doubleBE f)

doubleLE :: Double -> Builder
doubleLE f = Builder (Sum 8, BB.doubleLE f)

char7 :: Char -> Builder
char7 c = Builder (Sum 1, BB.char7 c)

string7 :: String -> Builder
string7 s = Builder (Sum (fromIntegral (length s)), BB.string7 s)

char8 :: Char -> Builder
char8 c = Builder (Sum 1, BB.char8 c)

string8 :: String -> Builder
string8 s = Builder (Sum (fromIntegral (length s)), BB.string8 s)

charUtf8 :: Char -> Builder
charUtf8 c = Builder (Sum (utf8Width c), BB.charUtf8 c)

stringUtf8 :: String -> Builder
stringUtf8 s = Builder (Sum (len 0 s), BB.stringUtf8 s)
  where
    len !n []      = n
    len !n (h : t) = len (n + utf8Width h) t
{-# INLINABLE stringUtf8 #-}
  -- INLINABLE so that if the input is constant, the
  -- compiler has the opportunity to precompute its length.

utf8Width :: Char -> Word
utf8Width c = case ord c of
  o | o <= 0x007F -> 1
    | o <= 0x07FF -> 2
    | o <= 0xFFFF -> 3
    | otherwise   -> 4
{-# INLINE utf8Width #-}
