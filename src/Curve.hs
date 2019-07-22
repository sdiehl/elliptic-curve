module Curve
  ( Curve(..)
  ) where

import Protolude

import Control.Monad.Random (MonadRandom)
import GaloisField (GaloisField)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Elliptic curves over Galois fields.
class GaloisField k => Curve r c k where
  {-# MINIMAL id, inv, add, def, disc, gen, order, point, rnd, yX #-}

  -- | Curve point.
  data family Point r c k :: *

  -- | Point identity.
  id :: Point r c k

  -- | Point inversion.
  inv :: Point r c k -> Point r c k

  -- | Point addition.
  add :: Point r c k -> Point r c k -> Point r c k

  -- | Point doubling.
  double :: Point r c k -> Point r c k
  double = join add
  {-# INLINE double #-}

  -- | Point multiplication.
  mul :: Integral n => Point r c k -> n -> Point r c k
  mul p n
    | n < 0     = inv (mul p (-n))
    | n == 0    = id
    | n == 1    = p
    | even n    = p'
    | otherwise = add p p'
    where
      p' = mul (double p) (div n 2)
  {-# INLINE mul #-}

  -- | Well-defined.
  def :: Point r c k -> Bool

  -- | Curve discriminant.
  disc :: Point r c k -> k

  -- | Curve generator.
  gen :: Point r c k

  -- | Curve order.
  order :: Point r c k -> Integer

  -- | Point from X coordinate.
  point :: k -> Maybe (Point r c k)

  -- | Random curve point.
  rnd :: MonadRandom m => m (Point r c k)

  -- | Y coordinate from X coordinate.
  yX :: Point r c k -> k -> Maybe k

-- Elliptic curves are semigroups.
instance Curve r c k => Semigroup (Point r c k) where
  (<>) = add

-- Elliptic curves are monoids.
instance Curve r c k => Monoid (Point r c k) where
  mempty = id
