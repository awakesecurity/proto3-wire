{-
  Copyright 2025 Arista Networks

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

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Presents right-associative folds as 'Foldable' sequences.
module Proto3.Wire.Encode.Repeated
  ( Unary(..)
  , foldMapRUnaryCount
  , foldMapLUnaryCount
  , Repeated(..)
  , nullRepeated
  , mapRepeated
  , reverseRepeated
  , reverseMapRepeated
  , forwardRepeatedFold
  , reverseRepeatedFold
  , ToRepeated(..)
  ) where

import Data.Functor.Identity (Identity(..))
import Data.IntMap.Internal qualified
import Data.IntMap.Lazy qualified
import Data.IntSet qualified
import Data.IntSet.Internal qualified
import Data.Kind (Type)
import Data.List.NonEmpty qualified
import Data.Map.Lazy qualified
import Data.Semigroup (Dual(..))
import Data.Sequence qualified
import Data.Set qualified
import Data.Vector qualified
import Data.Vector.Storable qualified
import Data.Vector.Unboxed qualified
import Foreign (Storable)
import GHC.Exts (Constraint, TYPE)
import GHC.Exts qualified
import Text.Read (Read(..))

-- | A lazy unary representation of a natural number--usually an element count.
data Unary = Zero | Succ Unary
  deriving stock (Eq, Show)

instance Semigroup Unary
  where
    -- | Adds unary numbers.  Because recursive functions do not inline,
    -- and hence might not beta-reduce across module boundaries, we avoid
    -- recursion in the algorithm until the result distinguishes between
    -- zero, one, and two or more.  Those are the three size categories
    -- that protobuf encoding must currently distinguish between.  Though
    -- we could use a type that explicitly ignores distinctions between
    -- two and three, we want to leave open the possibility of future
    -- applications demanding more size categories without necessitating
    -- a change in the programming interface.  In such an event we could
    -- unroll the non-recursive portion of this pattern match a bit more.
    Zero <> y = y
    Succ x <> y = Succ
      ( case x of
          Zero -> y
          Succ z -> Succ (recursivePlus z y)
            where
              recursivePlus Zero v = v
              recursivePlus (Succ u) v = Succ (recursivePlus u v)
      )

instance Monoid Unary
  where
    -- | The identity with respect to '<>' is 'Zero'.
    mempty = Zero

-- | Lazily counts the number of elements in a 'Foldable' sequence with
-- a 'foldMap'-style fold that assumes right-associativity is best.
--
-- NOTE: This function works well for right-associative data structures such
-- as lists.  It should also work fairly well for balanced binary trees,
-- though we can get slightly better efficiency with specialized recursions.
-- For left-associative data structures use 'foldMapLUnaryCount' instead.
foldMapRUnaryCount :: Foldable c => c e -> Unary
foldMapRUnaryCount xs = foldMap (\_ -> Succ Zero) xs
{-# INLINE foldMapRUnaryCount #-}

-- | Like 'foldMapRUnaryCount' but optimized to achieve weak head
-- normal form more quickly for left-associative data structures.
foldMapLUnaryCount :: Foldable c => c e -> Unary
foldMapLUnaryCount xs = getDual (foldMap (\_ -> Dual (Succ Zero)) xs)
{-# INLINE foldMapLUnaryCount #-}

-- | Expresses a sequence of values for encoding as a repeated field.
type Repeated :: forall er . TYPE er -> Type
data Repeated e = MkRepeated
  { countRepeated :: Either Unary Int
      -- ^ Optionally predicts the number of elements in the sequence.  Predict
      -- the count only when it is practical to do so accurately and quickly.
      --
      -- A prediction that is too low causes undefined behavior--possibly
      -- a crash.  A length prediction that is too high overallocates
      -- output space, as if the sequence really were that length.
      --
      -- When predicting the number of elements would be slow, this field
      -- instead provides a lazy and gradual answer to the question of how
      -- many elements are in the sequence.  Protobuf encoding may use this
      -- value to choose between various encoding options, and usually only
      -- needs to distinguish between zero elements, one element, and
      -- multiple elements.  Using 'foldMapRepeated' to answer this question
      -- could backfires if the fold direction is not optimal for the data
      -- structure, but note that a count does not care about direction, and
      -- therefore an optimal direction can be chosen during construction.
  , foldMapRepeated :: forall m . Monoid m => (e -> m) -> m
      -- ^ Performs a lazy 'foldMap' of the given function over the the desired
      -- sequence of field values.  Typically the monoid is a reverse builder
      -- such as `Proto3.Wire.Reverse.BuildR`, which will induce the fold to
      -- access elements in /reverse/ order.  It is best to make such access
      -- efficient, /but/ it is usually /not/ an optimization to allocate
      -- a reversed sequence just for that purpose.
      --
      -- Thanks to vector fusion rules, it is fast to appear to reverse a vector
      -- and then use 'Dual' to fold over that reversed vector while restoring
      -- the original order.  The actual effect will be to iterate backward
      -- through the existing vector, /not/ to allocate a reversed vector.
      --
      -- See also 'reverseRepeated', 'forwardRepeatedFold', and 'reverseRepeatedFold'.
  }
  deriving stock (Functor)

instance GHC.Exts.IsList (Repeated e)
  where
    type Item (Repeated e) = e
    fromList = toRepeated
    fromListN n xs = MkRepeated (Right n) (forwardRepeatedFold xs)
    toList (MkRepeated _ xs) = xs pure

instance Eq e =>
         Eq (Repeated e)
  where
    x == y = GHC.Exts.toList x == GHC.Exts.toList y

instance Read e =>
         Read (Repeated e)
  where
    readPrec = fmap GHC.Exts.fromList readListPrec

instance Show e =>
         Show (Repeated e)
  where
    showsPrec _ = showList . GHC.Exts.toList

-- | Is the given sequence empty?
nullRepeated :: Repeated e -> Bool
nullRepeated (MkRepeated count _) = case count of
  Left Zero -> True
  Left (Succ _) -> False
  Right n -> n == 0
{-# INLINE nullRepeated #-}

-- | A convenience function that maps a function over a sequence,
-- provided that the relevant types are all lifted.
mapRepeated ::
  forall (c :: Type) (e :: Type) (a :: Type) . ToRepeated c e => (e -> a) -> c -> Repeated a
mapRepeated f xs = fmap f (toRepeated xs)
{-# INLINE [1] mapRepeated #-}

-- | Reverses the semantic order of a 'Repeated' sequence, /without/ changing
-- the performance characteristics of the underlying source of the elements.
--
-- For example, @'reverseRepeated' ('toRepeated' "CBA")'@ is functionally
-- equivalent to @'toRepeated' "ABC"@, but performs better with builders
-- that start immediately with the semantically-final element, namely @\'C\'@,
-- because it remains the most accessible element of the underlying list.
reverseRepeated :: Repeated e -> Repeated e
reverseRepeated (MkRepeated count xs) = MkRepeated count (\f -> getDual (xs (\e -> Dual (f e))))
{-# INLINE [1] reverseRepeated #-}

-- Together, the following rules should tend to eliminate double reversals,
-- though it may be that GHC would optimize away their costs regardless,
-- provided that everything involved is at least INLINABLE.
{-# RULES
    "reverseRepeated/reverseRepeated"
      forall xs . reverseRepeated (reverseRepeated xs) = xs
  #-}
{-# RULES
    "mapRepeated/reverseRepeated"
      forall xs f . mapRepeated f (reverseRepeated xs) = reverseRepeated (mapRepeated f xs)
  #-}

-- | A convenient composition of 'reverseRepeated' and 'mapRepeated':
-- @reverseMapRepeated f xs = reverseRepeated (mapRepeated f xs)@
reverseMapRepeated ::
  forall (c :: Type) (e :: Type) (a :: Type) . ToRepeated c e => (e -> a) -> c -> Repeated a
reverseMapRepeated f xs = reverseRepeated (mapRepeated f xs)
{-# INLINE reverseMapRepeated #-}

-- | Converts from a (lifted) 'Foldable' sequence to the value for
-- field 'foldMapRepeated', preserving the order of that sequence.
--
-- This function is most efficient when the type of sequence you provide
-- makes it fast to start with the /last/ element of the provided sequence
-- (like a conceptually reversed vector) or at least has no particular
-- preference between first and last (like a `Set.Set`).
--
-- If instead your data structure makes it fastest to start with
-- the /first/ element of the sequence (like a @[]@), then consider
-- whether you can arrange for it to be in the reverse of the desired
-- order, and then apply 'reverseRepeated', which makes attempts to
-- access the semantically last element redirect to the first one.
-- But do not allocate a second sequence just for that purpose.
forwardRepeatedFold :: Foldable c => c e -> (forall m . Monoid m => (e -> m) -> m)
forwardRepeatedFold xs f = foldMap f xs
{-# INLINE forwardRepeatedFold #-}

-- | Like 'forwardRepeated', but reverses the semantic order of the sequence.
--
-- This function cannot actually change the underlying performance properties
-- of the sequence, so use this function only when it is fastest to start with
-- the /first/ element of the provided sequence--which should be in an order
-- that is opposite to the desired meaning of the 'Repeated' sequence.
--
-- See also the comments for 'foldMapRepeated'.
reverseRepeatedFold :: Foldable c => c e -> (forall m . Monoid m => (e -> m) -> m)
reverseRepeatedFold xs f = getDual (foldMap (\e -> Dual (f e)) xs)
{-# INLINE reverseRepeatedFold #-}

-- | For each container type, specifies the optimal method for reverse iteration.
type ToRepeated :: forall cr . TYPE cr -> forall er . TYPE er -> Constraint
class ToRepeated c e | c -> e
  where
    -- | Converts to a reverse iteration over the elements.
    toRepeated :: c -> Repeated e

instance forall er (e :: TYPE er) .
         ToRepeated (Repeated e) e
  where
    toRepeated = id
    {-# INLINE toRepeated #-}

instance ToRepeated (Identity a) a
  where
    toRepeated x = MkRepeated (Right 1) (\f -> f (runIdentity x))
    {-# INLINE toRepeated #-}

instance ToRepeated [a] a
  where
    toRepeated xs = MkRepeated (Left (foldMapRUnaryCount xs)) (forwardRepeatedFold xs)
      -- Unavoidably reads to the end of the list before presenting the last element.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.List.NonEmpty.NonEmpty a) a
  where
    toRepeated xs = MkRepeated (Left (foldMapRUnaryCount xs)) (forwardRepeatedFold xs)
      -- Unavoidably reads to the end of the list before presenting the last element.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Vector.Vector a) a
  where
    toRepeated xs = MkRepeated
      (Right (Data.Vector.length xs))
      (\f -> getDual (Data.Vector.foldMap (\e -> Dual (f e)) (Data.Vector.reverse xs)))
      -- Vector fusion should convert this to right-to-left iteration.
    {-# INLINE toRepeated #-}

instance Storable a =>
         ToRepeated (Data.Vector.Storable.Vector a) a
  where
    toRepeated xs = MkRepeated
      (Right (Data.Vector.Storable.length xs))
      ( \f -> getDual
          (Data.Vector.Storable.foldMap (\e -> Dual (f e)) (Data.Vector.Storable.reverse xs)) )
      -- Vector fusion should convert this to right-to-left iteration.
    {-# INLINE toRepeated #-}

instance Data.Vector.Unboxed.Unbox a =>
         ToRepeated (Data.Vector.Unboxed.Vector a) a
  where
    toRepeated xs = MkRepeated
      (Right (Data.Vector.Unboxed.length xs))
      ( \f -> getDual
          (Data.Vector.Unboxed.foldMap (\e -> Dual (f e)) (Data.Vector.Unboxed.reverse xs)) )
      -- Vector fusion should convert this to right-to-left iteration.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Sequence.Seq a) a
  where
    toRepeated xs = MkRepeated (Right (Data.Sequence.length xs)) (forwardRepeatedFold xs)
      -- Should present the last element without having to read through the whole sequence.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Set.Set a) a
  where
    toRepeated xs = MkRepeated (Right (Data.Set.size xs)) (forwardRepeatedFold xs)
      -- Should present the last element without having to read through the whole sequence.
    {-# INLINE toRepeated #-}

instance ToRepeated Data.IntSet.IntSet Int
  where
    toRepeated xs = MkRepeated
      (Left (countIntSet xs))
#if MIN_VERSION_containers(0,8,0)
      (\f -> Data.IntSet.foldMap f xs)
#else
      (\f -> Data.IntSet.foldl (\a x -> a <> f x) mempty xs)
#endif
      -- Should present the last element without having to read through the whole sequence.
    {-# INLINE toRepeated #-}

-- Note that weak head normal form does not demand recursive evaluation.
-- This property bounds the amount of the set that must be seen to decide
-- between size 0, size 1, and all other sizes.
countIntSet :: Data.IntSet.IntSet -> Unary
countIntSet xs = case xs of
#if MIN_VERSION_containers(0,8,0)
    Data.IntSet.Internal.Bin _ lf rt ->
#else
    Data.IntSet.Internal.Bin _ _ lf rt ->
#endif
      Succ (Succ (nonempty lf <> nonempty rt))
    Data.IntSet.Internal.Tip _ _ ->
      Succ (unaryFromInt (Data.IntSet.size xs - 1))
    Data.IntSet.Internal.Nil ->
      Zero
  where
    -- Assumes that the given set is nonempty and returns one less than its size.
    nonempty ys = case ys of
#if MIN_VERSION_containers(0,8,0)
      Data.IntSet.Internal.Bin _ lf rt ->
#else
      Data.IntSet.Internal.Bin _ _ lf rt ->
#endif
        Succ (nonempty lf <> nonempty rt)
      Data.IntSet.Internal.Tip _ _ ->
        unaryFromInt (Data.IntSet.size ys - 1)
      Data.IntSet.Internal.Nil ->
        error "countIntSet: nonempty branch"

instance ToRepeated (Data.Map.Lazy.Map k a) (k, a)
  where
    toRepeated xs = MkRepeated
      (Right (Data.Map.Lazy.size xs))
      (\f -> Data.Map.Lazy.foldMapWithKey (curry f) xs)
      -- Should present the last key-value pair without having to read through the whole map.
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.IntMap.Lazy.IntMap a) (Int, a)
  where
    toRepeated xs = MkRepeated
      (Left (countIntMap xs))
      (\f -> Data.IntMap.Lazy.foldMapWithKey (curry f) xs)
      -- Should present the last key-value pair without having to read through the whole map.
    {-# INLINE toRepeated #-}

-- Note that weak head normal form does not demand recursive evaluation.
-- This property bounds the amount of the map that must be seen to decide
-- between size 0, size 1, and all other sizes.
countIntMap :: Data.IntMap.Lazy.IntMap a -> Unary
countIntMap xs = case xs of
#if MIN_VERSION_containers(0,8,0)
    Data.IntMap.Internal.Bin _ lf rt ->
#else
    Data.IntMap.Internal.Bin _ _ lf rt ->
#endif
      Succ (Succ (nonempty lf <> nonempty rt))
    Data.IntMap.Internal.Tip _ _ ->
      Succ Zero
    Data.IntMap.Internal.Nil ->
      Zero
  where
    -- Assumes that the given map is nonempty and returns one less than its size.
    nonempty ys = case ys of
#if MIN_VERSION_containers(0,8,0)
      Data.IntMap.Internal.Bin _ lf rt ->
#else
      Data.IntMap.Internal.Bin _ _ lf rt ->
#endif
        Succ (nonempty lf <> nonempty rt)
      Data.IntMap.Internal.Tip _ _ ->
        Zero
      Data.IntMap.Internal.Nil ->
        error "countIntMap: nonempty branch"

-- | Creates a 'Unary' representation of @max 0@ of the given 'Int'.
unaryFromInt :: Int -> Unary
unaryFromInt n
  | n <= 0 = Zero
  | otherwise = Succ (unaryFromInt (n - 1))
