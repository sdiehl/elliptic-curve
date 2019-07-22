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
  {-# MINIMAL id, inv, add, def, disc, point, rnd #-}

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

  -- | Point is well-defined.
  def :: Point r c k -> Bool

  -- | Get curve discriminant.
  disc :: Point r c k -> k

  -- | Get point from x coordinate.
  point :: k -> Maybe (Point r c k)

  -- | Get random point.
  rnd :: MonadRandom m => m (Point r c k)

-- Elliptic curves are semigroups.
instance Curve r c k => Semigroup (Point r c k) where
  (<>) = add

-- Elliptic curves are monoids.
instance Curve r c k => Monoid (Point r c k) where
  mempty = id
