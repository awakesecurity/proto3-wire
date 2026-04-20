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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Presents right-associative folds as 'Foldable' sequences.
module Proto3.Wire.Encode.Repeated
  ( Repeated(..)
  , nullRepeated
  , toRepeated
  , reverseRepeated
  , mapRepeated
  , reverseMapRepeated
  , mapMaybeRepeated
  , foldMapRepeated
  , Reverse(..)
  , ToRepeated(..)
  ) where

import Control.Monad ((<=<))
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import Data.IntMap.Lazy qualified
import Data.IntSet qualified
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Lazy qualified
import Data.Monoid (Endo(..))
import Data.Semigroup (All(..), Dual(..))
import Data.Sequence qualified
import Data.Set qualified
import Data.Vector qualified
import Data.Vector.Storable qualified
import Data.Vector.Unboxed qualified
import Foreign (Storable)
import GHC.Exts qualified (IsList(..))
import Text.Read (Read(..))

-- | Expresses a sequence of values for encoding as a repeated field.
--
-- This type constructor is not 'Foldable' because it would not satisfy
-- the efficiency assumptions of that type class.  In particular, it is
-- optimized for left associativity.
data Repeated e
  where
    MkRepeated ::
      forall c a e .
      ToRepeated c a =>
      -- | Maps and optionally filters the elements of the sequence.
      --
      -- Note that mapping preserves the utility of 'predictRepeatedSource',
      -- whereas filtering invalidates such predictions.  Therefore it
      -- is best to avoid 'Left' values that always return 'Just'.
      Either (a -> Maybe e) (a -> e) ->
      -- | The container providing the sequence.
      c ->
      Repeated e

instance Functor Repeated
  where
    fmap f (MkRepeated (Left g) xs) = MkRepeated (Left (fmap f . g)) xs
    fmap f (MkRepeated (Right g) xs) = MkRepeated (Right (f . g)) xs
    {-# INLINE fmap #-}

instance GHC.Exts.IsList (Repeated e)
  where
    type Item (Repeated e) = e

    fromList xs = MkRepeated (Right id) xs
    {-# INLINE fromList #-}

    fromListN n xs = MkRepeated (Right id) (CountedList n xs)
    {-# INLINE fromListN #-}

    toList (MkRepeated (Left f) xs) = appEndo (foldMapRepeatedSource (Endo . maybe id (:) . f) xs) []
    toList (MkRepeated (Right f) xs) = appEndo (foldMapRepeatedSource (Endo . (:) . f) xs) []
    {-# INLINE toList #-}

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
nullRepeated (MkRepeated (Left f) xs) =
  getAll (getDual (foldMapRepeatedSource (foldMap (\_ -> Dual (All False)) . f) xs))
nullRepeated (MkRepeated (Right _) xs) =
  case predictRepeatedSource xs of
    Just c -> c <= 0
    Nothing -> getAll (getDual (foldMapRepeatedSource (\_ -> Dual (All False)) xs))
{-# INLINE nullRepeated #-}

-- | Converts to 'Repeated' from a sequence supporting 'ToRepeated'.
toRepeated :: ToRepeated c e => c -> Repeated e
toRepeated = mapRepeated id
{-# INLINE [1] toRepeated #-}
{-# RULES
    "toRepeated@Repeated"
      forall xs . toRepeated xs = xs
  #-}

-- | Equivalent to @'toRepeated' . 'Reverse'@.
reverseRepeated :: ToRepeated (Reverse c) e => c -> Repeated e
reverseRepeated = toRepeated . Reverse
{-# INLINE reverseRepeated #-}

-- | Maps a function over the elements of a 'Repeated' sequence.
mapRepeated :: ToRepeated c a => (a -> e) -> c -> Repeated e
mapRepeated f xs = MkRepeated (Right f) xs
{-# INLINE [1] mapRepeated #-}
{-# RULES
    "mapRepeated@Repeated"
      forall f xs . mapRepeated f xs = mapRepeatedRepeated f xs
  #-}

mapRepeatedRepeated :: (a -> e) -> Repeated a -> Repeated e
mapRepeatedRepeated f (MkRepeated g xs) = MkRepeated (bimap (fmap f .) (f .) g) xs
{-# INLINE mapRepeatedRepeated #-}

-- | Equivalent to @'mapRepeated' . 'Reverse'@.
reverseMapRepeated :: ToRepeated (Reverse c) a => (a -> e) -> c -> Repeated e
reverseMapRepeated = \f -> mapRepeated f . Reverse
{-# INLINE reverseMapRepeated #-}

-- | Maps and filters a 'Repeated' sequence.
mapMaybeRepeated :: ToRepeated c a => (a -> Maybe e) -> c -> Repeated e
mapMaybeRepeated f xs = MkRepeated (Left f) xs
{-# INLINE [1] mapMaybeRepeated #-}
{-# RULES
    "mapMaybeRepeated@Repeated"
      forall f xs . mapMaybeRepeated f xs = mapMaybeRepeatedRepeated f xs
  #-}

mapMaybeRepeatedRepeated :: (a -> Maybe e) -> Repeated a -> Repeated e
mapMaybeRepeatedRepeated f (MkRepeated g xs) = MkRepeated (Left (either (f <=<) (f .) g)) xs
{-# INLINE mapMaybeRepeatedRepeated #-}

foldMapRepeated :: (ToRepeated c e, Monoid m) => (e -> m) -> c -> m
foldMapRepeated f xs = foldMapRepeatedRepeated f (toRepeated xs)
{-# INLINE foldMapRepeated #-}

foldMapRepeatedRepeated :: Monoid m => (e -> m) -> Repeated e -> m
foldMapRepeatedRepeated g (MkRepeated (Left f) xs) = foldMapRepeatedSource (foldMap g . f) xs
foldMapRepeatedRepeated g (MkRepeated (Right f) xs) = foldMapRepeatedSource (g . f) xs
{-# INLINE [1] foldMapRepeatedRepeated #-}

-- | For each container type, specifies the optimal method for reverse iteration.
--
-- When instantiating this type class for a particular data structure, please also
-- instantiate it for 'Reverse' of that data structure, in the process exploiting
-- any special features that speed iteration in the indicated order.  (The exception
-- is the instance for 'Repeated' itself, for which there is no general reversal.)
--
-- See Also: 'reverseRepeated', 'reverseMapRepeated'
class ToRepeated c e | c -> e
  where
    -- | Optionally predicts the number of elements in the sequence.  Predict
    -- the count only when it is practical to do so accurately and quickly.
    --
    -- A prediction that is too low causes undefined behavior--possibly
    -- a crash.  A length prediction that is too high overallocates
    -- output space, as if the sequence really were that length, and may
    -- cause use of packed format where unpacked format would be smaller.
    -- And there may be other, unpredictable effects from incorrect
    -- predictions.  Therefore if you are in doubt, use 'Nothing'.
    predictRepeatedSource :: c -> Maybe Int

    -- | Performs a lazy 'foldMap' of the given function over the desired
    -- sequence of field values, preferably but not necessarily optimized
    -- so that demanding the elements in /reverse/ order is efficient
    -- (because we encode using a reverse builder).
    --
    -- The worst case arises when folding over a list because we must go
    -- to the end before we can build the last element, which is the one
    -- we must encode first.  In this case we build up calling context
    -- for the other elements.  But allocating a reversed list would be
    -- about the same overhead, so there is no point in doing that.  But
    -- if you can build the original list in reverse order to start with,
    -- then you can wrap the list in 'Reverse' to semantically reverse
    -- it while iterating in the natural order for a list data structure.
    --
    -- With vectors we can semantically reverse the vector and then use
    -- 'Dual' within our fold to restore the original order.  Thanks to
    -- vector fusion rules, the actual effect will be to iterate backward
    -- through the existing vector, /not/ to allocate a reversed vector.
    foldMapRepeatedSource :: Monoid m => (e -> m) -> c -> m

instance ToRepeated (Repeated e) e
  where
    predictRepeatedSource (MkRepeated (Left _) _) = Nothing
    predictRepeatedSource (MkRepeated (Right _) xs) = predictRepeatedSource xs
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource f (MkRepeated (Left g) xs) = foldMapRepeatedSource (foldMap f . g) xs
    foldMapRepeatedSource f (MkRepeated (Right g) xs) = foldMapRepeatedSource (f . g) xs
    {-# INLINE foldMapRepeatedSource #-}

-- | In the eyes of 'ToRepeated', reverses the order of a sequence.  But
-- this conceptual reversal cannot alter its performance characteristics.
--
-- For example, conceptually reversing @"CBA"@ is functionally
-- equivalent to using @"ABC"@ as-is, but performs better with reverse
-- builders because @\'C\'@ is the most accessible element of @"CBA"@.
--
-- We intentionally avoid defining a general instance of
-- @'ToRepeated' (Reverse c) e@ in terms of @'ToRepeated' c e@
-- because different data structures provide different options
-- for iterating in a particular order.  In particular, vectors
-- allow fast iteration in either order but we need to know that
-- they are vectors in order to exploit those features.
newtype Reverse c = Reverse c

-- | Note that @'Reverse' [e]@ performs better, but requires
-- that you arrange to build the list in reverse order.
instance ToRepeated [e] e
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = foldMap
      -- Reads to the end of the list before presenting the last element,
      -- but we think that explicitly reversing the list would be slower.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse [e]) e
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (foldMapRepeatedSource (Dual . f) xs)
    {-# INLINE foldMapRepeatedSource #-}

data CountedList e = CountedList Int [e]

instance ToRepeated (CountedList e) e
  where
    predictRepeatedSource (CountedList n _) = Just n
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (CountedList _ xs) -> foldMap f xs
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (CountedList e)) e
  where
    predictRepeatedSource (Reverse (CountedList n _)) = Just n
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource =
      \f (Reverse (CountedList _ xs)) -> getDual (foldMapRepeatedSource (Dual . f) xs)
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (NonEmpty e) e
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = foldMap
      -- Reads to the end of the list before presenting the last element,
      -- but we think that explicitly reversing the list would be slower.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (NonEmpty e)) e
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (foldMapRepeatedSource (Dual . f) xs)
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Identity a) a
  where
    predictRepeatedSource = \_ -> Just 1
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = coerce
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (Identity a)) a
  where
    predictRepeatedSource = \_ -> Just 1
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = coerce
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Data.Vector.Vector a) a
  where
    predictRepeatedSource = Just . Data.Vector.length
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource =
      \f xs -> getDual (Data.Vector.foldMap (Dual . f) (Data.Vector.reverse xs))
      -- Vector fusion should convert this to reverse iteration.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (Data.Vector.Vector a)) a
  where
    predictRepeatedSource = \(Reverse xs) -> Just (Data.Vector.length xs)
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (Data.Vector.foldMap (Dual . f) xs)
    {-# INLINE foldMapRepeatedSource #-}

instance Storable a =>
         ToRepeated (Data.Vector.Storable.Vector a) a
  where
    predictRepeatedSource = Just . Data.Vector.Storable.length
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource =
      \f xs -> getDual (Data.Vector.Storable.foldMap (Dual . f) (Data.Vector.Storable.reverse xs))
      -- Vector fusion should convert this to reverse iteration.
    {-# INLINE foldMapRepeatedSource #-}

instance Storable a =>
         ToRepeated (Reverse (Data.Vector.Storable.Vector a)) a
  where
    predictRepeatedSource = \(Reverse xs) -> Just (Data.Vector.Storable.length xs)
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (Data.Vector.Storable.foldMap (Dual . f) xs)
    {-# INLINE foldMapRepeatedSource #-}

instance Data.Vector.Unboxed.Unbox a =>
         ToRepeated (Data.Vector.Unboxed.Vector a) a
  where
    predictRepeatedSource = Just . Data.Vector.Unboxed.length
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource =
      \f xs -> getDual (Data.Vector.Unboxed.foldMap (Dual . f) (Data.Vector.Unboxed.reverse xs))
      -- Vector fusion should convert this to reverse iteration.
    {-# INLINE foldMapRepeatedSource #-}

instance Data.Vector.Unboxed.Unbox a =>
         ToRepeated (Reverse (Data.Vector.Unboxed.Vector a)) a
  where
    predictRepeatedSource = \(Reverse xs) -> Just (Data.Vector.Unboxed.length xs)
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (Data.Vector.Unboxed.foldMap (Dual . f) xs)
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Data.Sequence.Seq a) a
  where
    predictRepeatedSource = Just . Data.Sequence.length
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = foldMap
      -- Should present the last element without having to read through the whole sequence,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (Data.Sequence.Seq a)) a
  where
    predictRepeatedSource = \(Reverse xs) -> Just (Data.Sequence.length xs)
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (foldMap (Dual . f) xs)
      -- Should present the first element without having to read through the whole sequence,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Data.Set.Set a) a
  where
    predictRepeatedSource = Just . Data.Set.size
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = foldMap
      -- Should present the last element without having to read through the whole sequence,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (Data.Set.Set a)) a
  where
    predictRepeatedSource = \(Reverse xs) -> Just (Data.Set.size xs)
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (foldMap (Dual . f) xs)
      -- Should present the first element without having to read through the whole sequence,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated Data.IntSet.IntSet Data.IntSet.Key
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource =
#if MIN_VERSION_containers(0,8,0)
      Data.IntSet.foldMap
#else
      \f xs -> Data.IntSet.foldl (\a x -> a <> f x) mempty xs
#endif
      -- Should present the last element without having to read through the whole sequence,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse Data.IntSet.IntSet) Data.IntSet.Key
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource =
#if MIN_VERSION_containers(0,8,0)
      \f (Reverse xs) -> getDual (Data.IntSet.foldMap (Dual . f) xs
#else
      \f (Reverse xs) -> Data.IntSet.foldr (\x a -> a <> f x) mempty xs
#endif
      -- Should present the first element without having to read through the whole sequence,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Data.Map.Lazy.Map k a) (k, a)
  where
    predictRepeatedSource = Just . Data.Map.Lazy.size
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f -> Data.Map.Lazy.foldMapWithKey (curry f)
      -- Should present the last key-value pair without having to read through the whole map,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (Data.Map.Lazy.Map k a)) (k, a)
  where
    predictRepeatedSource = \(Reverse xs) -> Just (Data.Map.Lazy.size xs)
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource =
      \f (Reverse xs) -> getDual (Data.Map.Lazy.foldMapWithKey (curry (Dual . f)) xs)
      -- Should present the first key-value pair without having to read through the whole map,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Data.IntMap.Lazy.IntMap a) (Data.IntMap.Lazy.Key, a)
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f -> Data.IntMap.Lazy.foldMapWithKey (curry f)
      -- Should present the last key-value pair without having to read through the whole map,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (Data.IntMap.Lazy.IntMap a)) (Data.IntMap.Lazy.Key, a)
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource =
      \f (Reverse xs) -> getDual (Data.IntMap.Lazy.foldMapWithKey (curry (Dual . f)) xs)
      -- Should present the last key-value pair without having to read through the whole map,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}
