module Curve
  ( Curve(..)
  , Form(..)
  , Group(..)
  ) where

import Protolude

import GaloisField (GaloisField)

import Group (Group(..))

-------------------------------------------------------------------------------
-- Elliptic curve
-------------------------------------------------------------------------------

-- | Elliptic curves.
class (GaloisField k, Group (Point f c e k)) => Curve f c e k where
  {-# MINIMAL char, cof, disc, point, pointX, yX #-}

  -- | Curve point.
  data family Point f c e k :: *

  -- | Curve characteristic.
  char :: Point f c e k -> Integer

  -- | Curve cofactor.
  cof :: Point f c e k -> Integer

  -- | Curve discriminant.
  disc :: Point f c e k -> k

  -- | Point from X and Y coordinates.
  point :: k -> k -> Maybe (Point f c e k)

  -- | Point from X coordinate.
  pointX :: k -> Maybe (Point f c e k)

  -- | Y coordinate from X coordinate.
  yX :: Point f c e k -> k -> Maybe k

-- Elliptic curves are monoids.
instance Curve f c e k => Monoid (Point f c e k) where

  mempty = id
  {-# INLINE mempty #-}

-- Elliptic curves are semigroups.
instance Curve f c e k => Semigroup (Point f c e k) where

  p <> q = if p == q then dbl p else add p q
  {-# INLINE (<>) #-}

-- | Curve forms.
data Form = Binary
          | Edwards
          | Montgomery
          | Weierstrass
