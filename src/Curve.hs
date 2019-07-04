module Curve
  -- | Types
  ( Curve(..)
  ) where

import Protolude

import GaloisField (GaloisField)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Elliptic curves over Galois fields
class GaloisField k => Curve r c k where
  {-# MINIMAL id, inv, add, def #-}

  -- | Elliptic curve point
  data family Point r c k :: *

  -- | Point identity
  id :: Point r c k

  -- | Point inversion
  inv :: Point r c k -> Point r c k

  -- | Point addition
  add :: Point r c k -> Point r c k -> Point r c k

  -- | Point doubling
  double :: Point r c k -> Point r c k
  double = join add
  {-# INLINE double #-}

  -- | Point multiplication
  mul :: Integral n => n -> Point r c k -> Point r c k
  mul n p
    | n < 0     = inv (mul (-n) p)
    | n == 0    = id
    | n == 1    = p
    | even n    = p'
    | otherwise = add p p'
    where
      p' = mul (div n 2) (double p)
  {-# INLINE mul #-}

  -- | Point is well-defined
  def :: Point r c k -> Bool

-- | Elliptic curves are semigroups
instance Curve r c k => Semigroup (Point r c k) where
  (<>) = add

-- | Elliptic curves are monoids
instance Curve r c k => Monoid (Point r c k) where
  mempty = id
