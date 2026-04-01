{-
  Copyright 2025-2026 Arista Networks

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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Presents right-associative folds as 'Foldable' sequences.
module Proto3.Wire.Encode.Repeated
  ( Repeated(..)
  , nullRepeated
  , mapRepeated
  , reverseRepeated
  , reverseMapRepeated
  , ToRepeated(..)
  ) where

import Data.Bits qualified
import Data.Functor.Identity (Identity(..))
import Data.IntMap.Internal qualified
import Data.IntMap.Lazy qualified
import Data.IntSet qualified
import Data.IntSet.Internal qualified
import Data.Kind (Type)
import Data.List.NonEmpty qualified
import Data.Map.Lazy qualified
import Data.Semigroup (All(..), Dual(..))
import Data.Sequence qualified
import Data.Set qualified
import Data.Vector qualified
import Data.Vector.Storable qualified
import Data.Vector.Unboxed qualified
import Foreign (Storable)
import GHC.Exts (Constraint, TYPE)
import GHC.Exts qualified
import Text.Read (Read(..))

-- | Expresses a sequence of values for encoding as a repeated field.
--
-- This type constructor is not 'Foldable' because it would not satisfy
-- the efficiency assumptions of that type class.  In particular, it is
-- optimized for left associativity.
--
-- This type constructor supports unlifted types, but currently
-- most other features of this module support only lifted types.
type Repeated :: forall {er} . TYPE er -> Type
data Repeated e = MkRepeated
  { countRepeated :: Maybe Int
      -- ^ Optionally predicts the number of elements in the sequence.  Predict
      -- the count only when it is practical to do so accurately and quickly.
      --
      -- A prediction that is too low causes undefined behavior--possibly
      -- a crash.  A length prediction that is too high overallocates
      -- output space, as if the sequence really were that length, and may
      -- cause use of packed format where unpacked format would be smaller.
      -- And there may be other, unpredictable effects from incorrect
      -- predictions.  Therefore if you are in doubt, use 'Nothing'
  , unorderedRepeated :: forall m . Monoid m => (e -> m) -> m
    -- ^ Maps every element of the sequence to a monoidal value and appends
    -- those values an /unspecified order/ chosen for the degree to which it
    -- allows application of '<>' in a right-associative way.  We wish to
    -- expoit shortcuts such as @All False <> undefined = All False@.
    -- But do not allocate in order to force right associativity.
    --
    -- When 'countRepeated' is unpopulated, it is helpful for 'unorderedRepeated'
    -- to inline until at is clear that there are at least two elements in the
    -- sequence.  Avoiding recursion until that point speeds typical use cases
    -- (see below) because GHC will not beta-reduce recursive functions at
    -- compile time across module boundaries.  But such unrolling is only
    -- a preference--one that cannot be accomodated when filtering a sequence.
    --
    -- When 'countRepeated' is unpopulated, we often use 'unorderedRepeated' to
    -- choose between the different formats that are allowed for a packed repeated
    -- protobuf field.  Complete omission is of course optimal when there are zero
    -- elements.  For a single element, unpacked format is more compact because it
    -- avoids a length prefix.  Only when there are two or more elements does
    -- packet format fulfill its objective of shortening the encoding.
    --
    -- To distinguish the above three scenarios we can use a monoid that
    -- ignores further information once it has seen at least two elements.
    -- It should not care about the order of the elements.
    --
    -- By contrast, 'foldMapRepeated' is used for builders and therefore must fold
    -- elements in a particular order.  Furthermore, experience with prior versions
    -- of 'Repeated' whose builder folds guaranteed a particular associativity
    -- indicates that such guarantees trigger allocation of builder accumulators,
    -- even when using `GHC.Exts.oneShot` on the unapplied builder arguments.
    --
    -- Therefore we need both 'unorderedRepeated' and 'foldMapRepeated'; neither
    -- record field by itself is maximally efficient for both purposes.  And we
    -- cannot simply name which format to use for packed repeated protobuf fields
    -- because filtering changes the element count.  We need a way to recompute
    -- the appropriate format /after/ any filtering has been applied.
  , foldMapRepeated :: forall m . Monoid m => (e -> m) -> m
    -- ^ Performs a lazy 'foldMap' of the given function over the the desired
    -- sequence of field values.  Often the monoid is a reverse builder such
    -- as `Proto3.Wire.Reverse.BuildR`, which will induce the fold to access
    -- elements in /reverse/ order, even though it does /not/ reverse the
    -- semantic order.  It is best to make reverse iteration efficient,
    -- /but/ it is usually /not/ an optimization to allocate another
    -- sequence just for that purpose.
    --
    -- Thanks to vector fusion rules, it is fast to logically reverse a vector
    -- and then use 'Dual' to fold over that reversed vector while restoring
    -- the original order.  The actual effect will be to iterate backward
    -- through the existing vector, /not/ to allocate a reversed vector.
    --
    -- See also 'reverseRepeated'.
  }
  deriving stock (Functor)

countList :: [e] -> (forall m . Monoid m => (e -> m) -> m)
countList = \case
  [] -> \_ -> mempty
  [x] -> \j -> j x
  x1 : x2 : xs -> \j -> j x1 <> j x2 <> foldMap j xs
{-# INLINE countList #-}

instance GHC.Exts.IsList (Repeated e)
  where
    type Item (Repeated e) = e

    fromList xs = MkRepeated
      { countRepeated = Nothing
      , unorderedRepeated = countList xs
      , foldMapRepeated = \j -> foldMap j xs
          -- Reads to the end of the list before presenting the last element,
          -- but we think that explicitly reversing the list would be slower.
      }
    {-# INLINE fromList #-}

    fromListN n xs = MkRepeated
      { countRepeated = Just n
      , unorderedRepeated = \j -> foldMap j xs
          -- In this case we will not need 'unorderedRepeated' unless we filter,
          -- at which point the unrolling done by 'countList' will not help but
          -- might still increase code size.  Therefore we use a simple 'foldMap'.
      , foldMapRepeated = \j -> foldMap j xs
      }
    {-# INLINE fromListN #-}

    toList xs = foldMapRepeated xs pure

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
nullRepeated MkRepeated{ countRepeated, unorderedRepeated } = case countRepeated of
  Just n -> n == 0
  Nothing -> getAll (unorderedRepeated (\_ -> All False))
{-# INLINE nullRepeated #-}

-- | A convenience function that maps a function over a sequence,
-- provided that the relevant types are all lifted.
mapRepeated ::
  forall (c :: Type) (e :: Type) (a :: Type) . ToRepeated c e => (e -> a) -> c -> Repeated a
mapRepeated f xs = fmap f (toRepeated xs)
{-# INLINE mapRepeated #-}

-- | Reverses the semantic order of a 'Repeated' sequence, /without/ changing
-- the performance characteristics of the underlying source of the elements.
--
-- For example, @'reverseRepeated' ('toRepeated' "CBA")'@ is functionally
-- equivalent to @'toRepeated' "ABC"@, but performs better with builders
-- that start immediately with the semantically-final element, namely @\'C\'@,
-- because it remains the most accessible element of the underlying list.
reverseRepeated :: Repeated e -> Repeated e
reverseRepeated MkRepeated{ countRepeated, unorderedRepeated, foldMapRepeated } = MkRepeated
  { countRepeated
  , unorderedRepeated
  , foldMapRepeated = \j -> getDual (foldMapRepeated (\e -> Dual (j e)))
  }
{-# INLINE reverseRepeated #-}

-- | A convenient composition of 'reverseRepeated' and 'mapRepeated':
-- @reverseMapRepeated f xs = reverseRepeated (mapRepeated f xs)@
reverseMapRepeated ::
  forall (c :: Type) (e :: Type) (a :: Type) . ToRepeated c e => (e -> a) -> c -> Repeated a
reverseMapRepeated f xs = reverseRepeated (mapRepeated f xs)
{-# INLINE reverseMapRepeated #-}

-- | For each container type, specifies the optimal method for reverse iteration.
--
-- This type constructor supports unlifted types, but currently
-- most other features of this module support only lifted types.
type ToRepeated :: forall {cr} . TYPE cr -> forall {er} . TYPE er -> Constraint
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
    toRepeated (Identity x) = MkRepeated
      { countRepeated = Just 1
      , unorderedRepeated = \j -> j x
      , foldMapRepeated = \j -> j x
      }
    {-# INLINE toRepeated #-}

instance ToRepeated [a] a
  where
    toRepeated = GHC.Exts.fromList
    {-# INLINE toRepeated #-}

countNonEmpty :: Data.List.NonEmpty.NonEmpty e -> (forall m . Monoid m => (e -> m) -> m)
countNonEmpty = \case
  x Data.List.NonEmpty.:| [] -> \j -> j x
  x1 Data.List.NonEmpty.:| x2 : xs -> \j -> j x1 <> j x2 <> foldMap j xs
{-# INLINE countNonEmpty #-}

instance ToRepeated (Data.List.NonEmpty.NonEmpty a) a
  where
    toRepeated xs = MkRepeated
      { countRepeated = Nothing
      , unorderedRepeated = countNonEmpty xs
      , foldMapRepeated = \j -> foldMap j xs
          -- Reads to the end of the list before presenting the last element,
          -- but we think that explicitly reversing the list would be worse.
      }
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Vector.Vector a) a
  where
    toRepeated xs = MkRepeated
      { countRepeated = Just (Data.Vector.length xs)
      , unorderedRepeated = \j -> Data.Vector.foldMap j (Data.Vector.reverse xs)
          -- No need for unrolling because we are predicting the length.
          -- We could iterate in either order.  We choose reverse so that loop
          -- termination can test against the constant zero, reducing live values.
      , foldMapRepeated =
          \j -> getDual (Data.Vector.foldMap (\e -> Dual (j e)) (Data.Vector.reverse xs))
          -- Vector fusion should convert this to right-to-left iteration.
      }
    {-# INLINE toRepeated #-}

instance Storable a =>
         ToRepeated (Data.Vector.Storable.Vector a) a
  where
    toRepeated xs = MkRepeated
      { countRepeated = Just (Data.Vector.Storable.length xs)
      , unorderedRepeated = \j -> Data.Vector.Storable.foldMap j (Data.Vector.Storable.reverse xs)
          -- No need for unrolling because we are predicting the length.
          -- We could iterate in either order.  We choose reverse so that loop
          -- termination can test against the constant zero, reducing live values.
      , foldMapRepeated = \j -> getDual
          (Data.Vector.Storable.foldMap (\e -> Dual (j e)) (Data.Vector.Storable.reverse xs))
          -- Vector fusion should convert this to right-to-left iteration.
      }
    {-# INLINE toRepeated #-}

instance Data.Vector.Unboxed.Unbox a =>
         ToRepeated (Data.Vector.Unboxed.Vector a) a
  where
    toRepeated xs = MkRepeated
      { countRepeated = Just (Data.Vector.Unboxed.length xs)
      , unorderedRepeated = \j -> Data.Vector.Unboxed.foldMap j (Data.Vector.Unboxed.reverse xs)
          -- No need for unrolling because we are predicting the length.
          -- We could iterate in either order.  We choose reverse so that loop
          -- termination can test against the constant zero, reducing live values.
      , foldMapRepeated = \j -> getDual
          (Data.Vector.Unboxed.foldMap (\e -> Dual (j e)) (Data.Vector.Unboxed.reverse xs))
          -- Vector fusion should convert this to right-to-left iteration.
      }
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Sequence.Seq a) a
  where
    toRepeated xs = MkRepeated
      { countRepeated = Just (Data.Sequence.length xs)
      , unorderedRepeated = \j -> foldMap j xs
          -- No need for unrolling because we are predicting the length.
      , foldMapRepeated = \j -> foldMap j xs
          -- Should present the last element without having to read through the whole sequence,
          -- though we may have to descend to the bottom of the tree.
      }
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Set.Set a) a
  where
    toRepeated xs = MkRepeated
      { countRepeated = Just (Data.Set.size xs)
      , unorderedRepeated = \j -> foldMap j xs
          -- No need for unrolling because we are predicting the length.
      , foldMapRepeated = \j -> foldMap j xs
          -- Should present the last element without having to read through the whole sequence,
          -- though we may have to descend to the bottom of the tree.
      }
    {-# INLINE toRepeated #-}

countIntSet :: Data.IntSet.IntSet -> (forall m . Monoid m => (Data.IntSet.Key -> m) -> m)
countIntSet xs j = case xs of
#if MIN_VERSION_containers(0,8,0)
  Data.IntSet.Internal.Bin _ lf rt ->
#else
  Data.IntSet.Internal.Bin _ _ lf rt ->
#endif
    -- Note that the inline part of this function presents the existence of
    -- two elements, though it requires recursion to provide their values.
    let (l, ls) = Data.IntSet.deleteFindMin lf
        (r, rs) = Data.IntSet.deleteFindMax rt
    in j l <> j r <> Data.IntSet.foldr ((<>) . j) (Data.IntSet.foldr ((<>) . j) mempty rs) ls
  Data.IntSet.Internal.Tip _ m ->
    case Data.Bits.popCount m of
      1 -> let z = case Data.IntSet.toList xs of
                     [] -> error "countIntSet: singleton Tip folds to zero elements"
                     [y] -> y
                     _ : _ : _ -> error "countIntSet: singleton Tip folds to multiple elements"
           in j z
      _ -> let (z1, z2, zs) = case Data.IntSet.toList xs of
                                [] -> error "countIntSet: non-singleton Tip folds to zero elements"
                                [_] -> error "countIntSet: non-singleton Tip folds to one element"
                                y1 : y2 : ys -> (y1, y2, ys)
           in j z1 <> j z2 <> foldMap j zs
  Data.IntSet.Internal.Nil ->
    mempty
{-# INLINE countIntSet #-}

instance ToRepeated Data.IntSet.IntSet Data.IntSet.Key
  where
    toRepeated xs = MkRepeated
      { countRepeated = Nothing
      , unorderedRepeated = countIntSet xs
      , foldMapRepeated =
#if MIN_VERSION_containers(0,8,0)
          \j -> Data.IntSet.foldMap j xs
#else
          \j -> Data.IntSet.foldl (\a x -> a <> j x) mempty xs
#endif
          -- Should present the last element without having to read through the whole sequence,
          -- though we may have to descend to the bottom of the tree.
      }
    {-# INLINE toRepeated #-}

instance ToRepeated (Data.Map.Lazy.Map k a) (k, a)
  where
    toRepeated xs = MkRepeated
      { countRepeated = Just (Data.Map.Lazy.size xs)
      , unorderedRepeated = \j -> Data.Map.Lazy.foldMapWithKey (curry j) xs
          -- No need for unrolling because we are predicting the length.
      , foldMapRepeated = \j -> Data.Map.Lazy.foldMapWithKey (curry j) xs
          -- Should present the last key-value pair without having to read through the whole map,
          -- though we may have to descend to the bottom of the tree.
      }
    {-# INLINE toRepeated #-}

countIntMap ::
  Data.IntMap.Lazy.IntMap a ->
  (forall m . Monoid m => ((Data.IntMap.Lazy.Key, a) -> m) -> m)
countIntMap xs j = case xs of
#if MIN_VERSION_containers(0,8,0)
  Data.IntMap.Internal.Bin _ lf rt ->
#else
  Data.IntMap.Internal.Bin _ _ lf rt ->
#endif
    -- Note that the inline part of this function presents the existence of
    -- two elements, though it requires recursion to provide their values.
    let (l, ls) = Data.IntMap.Lazy.deleteFindMin lf
        (r, rs) = Data.IntMap.Lazy.deleteFindMax rt
    in j l <> j r <>
         Data.IntMap.Lazy.foldMapWithKey (curry j) ls <>
         Data.IntMap.Lazy.foldMapWithKey (curry j) rs
  Data.IntMap.Internal.Tip k x ->
    j (k, x)
  Data.IntMap.Internal.Nil ->
    mempty
{-# INLINE countIntMap #-}

instance ToRepeated (Data.IntMap.Lazy.IntMap a) (Data.IntMap.Lazy.Key, a)
  where
    toRepeated xs = MkRepeated
      { countRepeated = Nothing
      , unorderedRepeated = countIntMap xs
      , foldMapRepeated = \j -> Data.IntMap.Lazy.foldMapWithKey (curry j) xs
          -- Should present the last key-value pair without having to read through the whole map,
          -- though we may have to descend to the bottom of the tree.
      }
    {-# INLINE toRepeated #-}
