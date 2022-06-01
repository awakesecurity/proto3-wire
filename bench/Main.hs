{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Proto3.Wire.Decode as De
import qualified Proto3.Wire.Encode as En
import Proto3.Wire

import Control.Applicative (liftA3)
import Data.Maybe
import Data.Word
import Data.IORef

import Criterion (bench)
import qualified Criterion as C
import Criterion.Main (defaultMain)

data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Eq, Functor)

instance Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Branch a t1 t2) = foldr f (f a (foldr f z t2)) t1

  sum Leaf = 0
  sum (Branch a t1 t2) =
    let !a1 = sum t1
        !a2 = sum t2
    in a + a1 + a2

intTreeParser :: De.Parser De.RawMessage (Tree Word64)
intTreeParser = liftA3 combine
    (De.at (De.repeated De.fixed64) (FieldNumber 0))
    (De.at (De.one (De.embedded' intTreeParser) Leaf) (FieldNumber 1))
    (De.at (De.one (De.embedded' intTreeParser) Leaf) (FieldNumber 2))
  where
    combine xs y z = Branch (sum xs) y z

detRandom :: [Word64]
detRandom = concat . replicate 10 $
  [ 227, 133, 16, 164, 43,
    159, 207, 87, 180, 236,
    245, 128, 249, 170, 216,
    181, 164, 162, 239, 249,
    76, 237, 197, 246, 209,
    231, 124, 154, 55, 64,
    4, 114, 79, 199, 252,
    163, 116, 237, 209, 138,
    240, 148, 212, 224, 88,
    131, 122, 114, 158, 97,
    186, 3, 223, 230, 223,
    207, 93, 168, 48, 130,
    77, 122, 30, 222, 221,
    224, 243, 19, 175, 61,
    112, 246, 201, 57, 185,
    19, 128, 129, 138, 209,
    4, 153, 196, 238, 72,
    254, 157, 233, 81, 30,
    106, 249, 57, 214, 104,
    171, 146, 175, 185, 192,
    159, 207, 87, 180, 236,
    227, 133, 16, 164, 43,
    245, 128, 249, 170, 216,
    181, 164, 162, 239, 249,
    76, 237, 197, 246, 209,
    231, 124, 154, 55, 64,
    4, 114, 79, 199, 252,
    163, 116, 237, 209, 138,
    240, 148, 212, 224, 88,
    131, 122, 114, 158, 97,
    186, 3, 223, 230, 223,
    207, 93, 168, 48, 130,
    77, 122, 30, 222, 221,
    224, 243, 19, 175, 61,
    112, 246, 201, 57, 185,
    19, 128, 129, 138, 209,
    4, 153, 196, 238, 72,
    254, 157, 233, 81, 30,
    106, 249, 57, 214, 104,
    171, 146, 175, 185, 192,
    159, 207, 87, 180, 236,
    227, 133, 16, 164, 43,
    245, 128, 249, 170, 216,
    181, 164, 162, 239, 249,
    76, 237, 197, 246, 209,
    231, 124, 154, 55, 64,
    4, 114, 79, 199, 252,
    163, 116, 237, 209, 138,
    240, 148, 212, 224, 88,
    131, 122, 114, 158, 97,
    186, 3, 223, 230, 223,
    207, 93, 168, 48, 130,
    77, 122, 30, 222, 221,
    224, 243, 19, 175, 61,
    112, 246, 201, 57, 185,
    19, 128, 129, 138, 209,
    4, 153, 196, 238, 72,
    254, 157, 233, 81, 30,
    106, 249, 57, 214, 104,
    171, 146, 175, 185, 192
    ]

pullInt :: IORef [Word64] -> IO Word64
pullInt xs = do
  xs' <- readIORef xs
  case xs' of
    [] -> pure 7
    x : xs' -> do
      writeIORef xs xs'
      pure x

mkTree0 :: IORef [Word64] -> IO En.MessageBuilder
mkTree0 ints = do
  shouldFork <- (\(i :: Word64) -> (i `mod` 8) < 6) <$> pullInt ints
  if shouldFork
    then do
      i <- En.fixed64 (FieldNumber 0) <$> pullInt ints
      left <- En.embedded (FieldNumber 1) <$> mkTree0 ints
      right <- En.embedded (FieldNumber 2) <$> mkTree0 ints
      pure (i <> left <> right)
    else pure mempty

mkTree :: IO B.ByteString
mkTree = BL.toStrict . En.toLazyByteString <$> (mkTree0 =<< newIORef detRandom)

decode :: B.ByteString -> IO (Maybe Word64)
decode = pure . fmap sum . toMaybe . De.parse intTreeParser
  where
    toMaybe (Left _) = Nothing
    toMaybe (Right x) = Just x

main :: IO ()
main =
  defaultMain
    [ bench "Parse int tree" $ C.perRunEnv mkTree decode ]
