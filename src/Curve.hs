module Curve
  ( Curve(..)
  , Form(..)
  , Group(..)
  ) where

import Protolude

import GaloisField (GaloisField)
import PrimeField (PrimeField, toInt)

import Group (Group(..))

-------------------------------------------------------------------------------
-- Elliptic curve
-------------------------------------------------------------------------------

-- | Elliptic curves.
class (GaloisField q, GaloisField r, Group (Point f c e q r))
  => Curve f c e q r where
  {-# MINIMAL char, cof, disc, point, pointX, yX #-}

  -- | Curve point.
  data family Point f c e q r :: *

  -- | Curve characteristic.
  char :: Point f c e q r -> Integer

  -- | Curve cofactor.
  cof :: Point f c e q r -> Integer

  -- | Curve discriminant.
  disc :: Point f c e q r -> q

  -- | Point multiplication.
  mul :: r ~ PrimeField p => Point f c e q r -> r -> Point f c e q r
  mul = (. toInt) . mul'
  {-# INLINE mul #-}

  -- | Point from X and Y coordinates.
  point :: q -> q -> Maybe (Point f c e q r)

  -- | Point from X coordinate.
  pointX :: q -> Maybe (Point f c e q r)

  -- | Y coordinate from X coordinate.
  yX :: Point f c e q r -> q -> Maybe q

-- Elliptic curves are monoids.
instance Curve f c e q r => Monoid (Point f c e q r) where

  mempty = id
  {-# INLINE mempty #-}

-- Elliptic curves are semigroups.
instance Curve f c e q r => Semigroup (Point f c e q r) where

  p <> q = if p == q then dbl p else add p q
  {-# INLINE (<>) #-}

-- | Curve forms.
data Form = Binary
          | Edwards
          | Montgomery
          | Weierstrass
