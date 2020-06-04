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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Augmentations to type classes such as 'Semigroup' and 'Monoid' that may
-- be used to track the type-level width information of builder primitives.
module Proto3.Wire.Reverse.Width
  ( SemigroupNat(..)
  , MonoidNat(..)
  , Max
  , ChooseNat(..)
  ) where

import Data.Type.Bool (If)
import GHC.Exts (Proxy#)
import GHC.TypeLits (KnownNat, type (<=?), type (+))

-- | Provides an associative operator that adds the type-level width.
class SemigroupNat b
  where
    -- | Associates to the left because we build in reverse.
    (>+<) :: forall v w . (KnownNat v, KnownNat w) => b v -> b w -> b (v + w)
    infixl 6 >+<

    assocLPlusNat ::
      forall u v w . Proxy# '(u, v, w) -> b (u + (v + w)) -> b ((u + v) + w)

    assocRPlusNat ::
      forall u v w . Proxy# '(u, v, w) -> b ((u + v) + w) -> b (u + (v + w))

    commPlusNat ::
      forall v w . Proxy# '(v, w) -> b (v + w) -> b (w + v)

-- | Provides a zero-width identity for '>+<'.
--
-- (An alternative would be a `Control.Category.Category` in which
-- an offset change is specified instead of the underlying width,
-- but this approach leads to more straightforward builder types.)
class SemigroupNat b =>
      MonoidNat b
  where
    memptyNat :: b 0

-- | The larger of two `GHC.TypeLits.Nat`s.
type Max v w = If (w <=? v) v w

-- | Chooses between alternatives based on a condition.
--
-- Note that while this type class makes sense for bounded builder primitives,
-- it should not be instantiated for fixed-width primitives because the choice
-- between alternatives introduces a run-time variation in width.
class ChooseNat b
  where
    -- | Like `Data.Bool.bool`, chooses the first argument on 'False'
    -- and the second on 'True', either way promoting the type-level
    -- `GHC.TypeLits.Nat` to the larger of the given `GHC.TypeLits.Nat`s.
    --
    -- Defaults to the natural implementation in terms of 'ifNat'.
    boolNat ::
      forall v w. (KnownNat v, KnownNat w) => b v -> b w -> Bool -> b (Max v w)
    boolNat f t b = ifNat b t f
    {-# INLINE CONLIKE boolNat #-}

    -- | Like @if _ then _ else@, chooses the first argument on 'True'
    -- and the second on 'False', either way promoting the type-level
    -- `GHC.TypeLits.Nat` to the larger of the given `GHC.TypeLits.Nat`s.
    --
    -- Defaults to the natural implementation in terms of 'boolNat'.
    ifNat ::
      forall v w. (KnownNat v, KnownNat w) => Bool -> b v -> b w -> b (Max w v)
    ifNat c t e = boolNat e t c
    {-# INLINE CONLIKE ifNat #-}

    assocLMaxNat ::
      forall u v w .
      Proxy# '(u, v, w) -> b (Max u (Max v w)) -> b (Max (Max u v) w)

    assocRMaxNat ::
      forall u v w .
      Proxy# '(u, v, w) -> b (Max (Max u v) w) -> b (Max u (Max v w))

    commMaxNat ::
      forall v w . Proxy# '(v, w) -> b (Max v w) -> b (Max w v)
