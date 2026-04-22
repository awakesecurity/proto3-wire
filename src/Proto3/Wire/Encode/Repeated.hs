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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Presents right-associative folds as 'Foldable' sequences.
module Proto3.Wire.Encode.Repeated
  ( Repeated
  , nullRepeated
  , predictRepeated
  , foldMapRepeated
  , foldMapRepeated'
  , toRepeated
  , mapRepeated
  , mapMaybeRepeated
  , mapFoldRepeated
  , Reverse(..)
  , Count(Count, ..)
  , ToRepeated(..)
  ) where

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
import GHC.Exts (inline, oneShot)
import GHC.Exts qualified (IsList(..))
import Text.Read (Read(..))

import Data.Foldable (foldMap')
import Data.Semigroup (Sum(..))

-- | Expresses a sequence of values for encoding as a repeated field.
--
-- This type constructor is not 'Foldable' because it would not satisfy
-- the efficiency assumptions of that type class.  In particular, it is
-- optimized for left associativity.
data Repeated e
  where
    MapRepeated ::
      forall c a e .
      ToRepeated c a =>
      -- | Maps the elements of the sequence individually.
      -- /without/ modifing the number or order of elements.
      --
      -- In this case 'predictRepeatedSource' remains useful.
      (a -> e) ->
      -- | The container providing the sequence.
      c ->
      Repeated e

    BindRepeated ::
      forall c a e .
      ToRepeated c a =>
      -- | Maps each element of the sequence to zero or more new
      -- elements and concatenates the results by transforming
      -- the fold to be passed to 'foldMapRepeatedSource'.
      --
      -- We cannot make use of 'predictRepeatedSource' in this case.
      (forall m . Monoid m => (e -> m) -> a -> m) ->
      -- | The container providing the sequence.
      c ->
      Repeated e

instance Functor Repeated
  where
    fmap f (MapRepeated g xs) = MapRepeated (\x -> f (g x)) xs
    fmap f (BindRepeated g xs) = BindRepeated (\j x -> g (\y -> j (f y)) x) xs
    {-# INLINE fmap #-}

instance GHC.Exts.IsList (Repeated e)
  where
    type Item (Repeated e) = e

    fromList xs = MapRepeated id xs

    fromListN n xs = MapRepeated id (UnsafeCount n xs)

    toList (MapRepeated g xs) = appEndo (foldMapRepeatedSource (\x -> Endo (g x :)) xs) []
    toList (BindRepeated g xs) = appEndo (foldMapRepeatedSource (\x -> g (\y -> Endo (y :)) x) xs) []

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
nullRepeated (MapRepeated _ xs) =
  case predictRepeatedSource xs of
    Just c -> c <= 0
    Nothing -> getAll (getDual (foldMapRepeatedSource (\_ -> Dual (All False)) xs))
nullRepeated (BindRepeated g xs) =
  getAll (getDual (foldMapRepeatedSource (\x -> g (\_ -> Dual (All False)) x) xs))
{-# INLINE nullRepeated #-}

-- | May predict the number of elements in the sequence, but does
-- so only when it is practical to do so accurately and quickly.
--
-- More specifically, this function delegates to 'predictRepeatedSource'
-- but only when the source sequence has not been filtered by functions
-- such as 'mapMaybeRepeated', and 'predictRepeatedSource' may itself
-- decline to predict the number of elements in the source sequence.
--
-- For example, it is easy to predict the length of a vector, but
-- we would have to prescan a lazy list to discover its length.
predictRepeated :: ToRepeated c e => c -> Maybe Int
predictRepeated = predictRepeatedSource . toRepeated
{-# INLINE predictRepeated #-}

-- | Equivalent to a lazy 'foldMap' over the given sequence.
foldMapRepeated :: (ToRepeated c e, Monoid m) => (e -> m) -> c -> m
foldMapRepeated f = foldMapRepeatedSource f . toRepeated
{-# INLINE foldMapRepeated #-}

-- | Like 'foldMapRepeated', but strictly accumulates from the end of the sequence.
foldMapRepeated' :: (ToRepeated c e, Monoid m) => (e -> m) -> c -> m
foldMapRepeated' f = \xs ->
  foldMapRepeated (\x -> Endo (oneShot (\acc -> acc `seq` f x <> acc))) xs `appEndo` mempty
    -- Curiously, a newtype around the 'Monoid' that strictifies the right operand
    -- is insufficient to cause GHC 9.8.2 to pass the accumulator to recursive calls
    -- instead of applying '<>' after making the recursive call.  It is not clear why.
    -- That is why we instead use 'Endo' here.
{-# INLINE foldMapRepeated' #-}

-- | Converts to 'Repeated' from a sequence supporting 'ToRepeated'.
toRepeated :: ToRepeated c e => c -> Repeated e
toRepeated = MapRepeated id
{-# INLINE [1] toRepeated #-}
{-# RULES "toRepeated@Repeated" toRepeated = id #-}

-- | Maps a function over the elements of a 'Repeated' sequence.
mapRepeated :: ToRepeated c a => (a -> e) -> c -> Repeated e
mapRepeated f = fmap f . toRepeated
{-# INLINE mapRepeated #-}

-- | Maps and filters a 'Repeated' sequence, with
-- the same semantics as `Data.Maybe.mapMaybe`.
--
-- Necessarily invalidates any predicted number of elements.
mapMaybeRepeated :: ToRepeated c a => (a -> Maybe e) -> c -> Repeated e
mapMaybeRepeated f = mapFoldRepeated (\j a -> foldMap j (f a))
{-# INLINE mapMaybeRepeated #-}

-- | Maps each element of the sequence to zero or more new
-- elements and concatenates the results by transforming
-- the fold to be passed to 'foldMapRepeatedSource'.
--
-- The semantics are similar to 'foldMap' and 'foldMapRepeated',
-- but in this case the result is another 'Repeated' rather than
-- a final monoidal result.  (Though conceptually one can view
-- 'Repeated' as a 'Monoid' under concatenation, in practice
-- we have not yet implemented such an operation.)
--
-- NOTE: As with 'foldMapRepeatedSource', it is preferred that
-- the fold transformation that you pass to this function allow
-- the fold that is eventually passed in to demand the elements
-- that it expects in /reverse/ order without harming efficiency
-- (because that fold typically creates a reverse builder).
--
-- For example, the given fold transformer might create a subsequence
-- of new elements from a single element of the original sequence,
-- then use 'foldMapRepeated' on that subsequence in order to present
-- the new elements in reverse order to the extent that is practical.
--
-- Necessarily invalidates any predicted number of elements.
--
-- For example, @mapMaybeRepeated f = mapFoldRepeated (\h -> foldMap h . f)@.
mapFoldRepeated :: ToRepeated c a => (forall m . Monoid m => (e -> m) -> a -> m) -> c -> Repeated e
mapFoldRepeated f = \xs -> case toRepeated xs of
  MapRepeated g ys -> BindRepeated (\j y -> inline (f (inline j) (inline (g y)))) ys
  BindRepeated g ys -> BindRepeated (\j y -> inline (g (inline f (\e -> inline (j e))) y)) ys
{-# INLINE mapFoldRepeated #-}

-- | For each container type, specifies the optimal method for reverse iteration.
--
-- When instantiating this type class for a particular data structure, please also
-- instantiate it for 'Reverse' of that data structure, in the process exploiting
-- any special features that speed iteration in the indicated order.  (The exception
-- is the instance for 'Repeated' itself, for which there is no general reversal.)
--
-- See Also: 'Reverse'
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
    predictRepeatedSource (MapRepeated _ ys) = predictRepeatedSource ys
    predictRepeatedSource (BindRepeated _ _) = Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource f (MapRepeated g ys) = foldMapRepeatedSource (\y -> f (g y)) ys
    foldMapRepeatedSource f (BindRepeated g ys) = foldMapRepeatedSource (\y -> g f y) ys
    {-# INLINE foldMapRepeatedSource #-}

-- | As viewed through 'ToRepeated', reverses the order of a sequence.  But
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

-- | As viewed through 'ToRepeated', predicts the number of elements in a sequence,
-- replacing the behavior of 'predictRepeatedSource' for the underlying sequence.
--
-- For example, if you happen to know the length of a list specifying the elements
-- of a repeated field of fixed-width type, you can improve encoder performance by
-- providing that information to the encoder by means of this wrapper.
--
-- 'Count' should be the outer wrapper if 'Reverse' is also used.  That way
-- the generic instance of 'ToRepeated' for 'Counter' can apply, regardless
-- of the more specific instance for 'Reverse' of a specific sequence type.
data Count c = UnsafeCount Int c
  -- ^ This data constructor is unsafe because it /ASSUMES/ the element count is accurate.
  -- See 'predictRepeatedSource' for what can happen if this count is incorrect.

{-# COMPLETE Count #-}

pattern Count :: Int -> c -> Count c
pattern Count n c <- UnsafeCount n c

instance ToRepeated c e =>
         ToRepeated (Count c) e
  where
    predictRepeatedSource = \(Count n _) -> Just n
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Count _ xs) -> foldMapRepeatedSource f xs
    {-# INLINE foldMapRepeatedSource #-}

-- | Presents the elements of a list in order.
--
-- Note that @'Reverse' [e]@ performs better, but
-- requires you to build the list in reverse order.
instance ToRepeated [e] e
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = foldMap
      -- Reads to the end of the list before presenting the last element,
      -- but we think that explicitly reversing the list would be slower.
    {-# INLINE foldMapRepeatedSource #-}

-- | Presents the elements of a list in /reverse/ order.
--
-- Performs better than plain @[e]@, but requires
-- that you to build the list in reverse order.
instance ToRepeated (Reverse [e]) e
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (foldMap (\x -> Dual (f x)) xs)
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

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (foldMap (\x -> Dual (f x)) xs)
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

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (Data.Vector.foldMap (\x -> Dual (f x)) xs)
    {-# INLINE foldMapRepeatedSource #-}

instance Storable a =>
         ToRepeated (Data.Vector.Storable.Vector a) a
  where
    predictRepeatedSource = Just . Data.Vector.Storable.length
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f xs ->
      getDual (Data.Vector.Storable.foldMap (\x -> Dual (f x)) (Data.Vector.Storable.reverse xs))
      -- Vector fusion should convert this to reverse iteration.
    {-# INLINE foldMapRepeatedSource #-}

instance Storable a =>
         ToRepeated (Reverse (Data.Vector.Storable.Vector a)) a
  where
    predictRepeatedSource = \(Reverse xs) -> Just (Data.Vector.Storable.length xs)
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) ->
      getDual (Data.Vector.Storable.foldMap (\x -> Dual (f x)) xs)
    {-# INLINE foldMapRepeatedSource #-}

instance Data.Vector.Unboxed.Unbox a =>
         ToRepeated (Data.Vector.Unboxed.Vector a) a
  where
    predictRepeatedSource = Just . Data.Vector.Unboxed.length
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f xs ->
      getDual (Data.Vector.Unboxed.foldMap (\x -> Dual (f x)) (Data.Vector.Unboxed.reverse xs))
      -- Vector fusion should convert this to reverse iteration.
    {-# INLINE foldMapRepeatedSource #-}

instance Data.Vector.Unboxed.Unbox a =>
         ToRepeated (Reverse (Data.Vector.Unboxed.Vector a)) a
  where
    predictRepeatedSource = \(Reverse xs) -> Just (Data.Vector.Unboxed.length xs)
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) ->
      getDual (Data.Vector.Unboxed.foldMap (\x -> Dual (f x)) xs)
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

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (foldMap (\x -> Dual (f x)) xs)
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

    foldMapRepeatedSource = \f (Reverse xs) -> getDual (foldMap (\x -> Dual (f x)) xs)
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
      \f (Reverse xs) -> getDual (Data.IntSet.foldMap (\x -> Dual (f x)) xs
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

    foldMapRepeatedSource = \f -> Data.Map.Lazy.foldMapWithKey (\k v -> f (k, v))
      -- Should present the last key-value pair without having to read through the whole map,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (Data.Map.Lazy.Map k a)) (k, a)
  where
    predictRepeatedSource = \(Reverse xs) -> Just (Data.Map.Lazy.size xs)
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) ->
      getDual (Data.Map.Lazy.foldMapWithKey (\k v -> Dual (f (k, v))) xs)
      -- Should present the first key-value pair without having to read through the whole map,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Data.IntMap.Lazy.IntMap a) (Data.IntMap.Lazy.Key, a)
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f -> Data.IntMap.Lazy.foldMapWithKey (\k v -> f (k, v))
      -- Should present the last key-value pair without having to read through the whole map,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}

instance ToRepeated (Reverse (Data.IntMap.Lazy.IntMap a)) (Data.IntMap.Lazy.Key, a)
  where
    predictRepeatedSource = \_ -> Nothing
    {-# INLINE predictRepeatedSource #-}

    foldMapRepeatedSource = \f (Reverse xs) ->
      getDual (Data.IntMap.Lazy.foldMapWithKey (\k v -> Dual (f (k, v))) xs)
      -- Should present the last key-value pair without having to read through the whole map,
      -- though we may have to descend to the bottom of the tree.
    {-# INLINE foldMapRepeatedSource #-}
