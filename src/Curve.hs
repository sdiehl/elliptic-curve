module Curve
  ( module Curve
  , module Group
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField)
import PrimeField (PrimeField, toInt)
import Test.Tasty.QuickCheck (Arbitrary(..))

import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Elliptic curves.
class (forall p . (KnownNat p, r ~ PrimeField p),
  GaloisField q, GaloisField r, Group (Point f c e q r))
  => Curve (f :: Form) (c :: Coordinates) e q r where
  {-# MINIMAL char, cof, disc, fromA, point, pointX, toA, yX #-}

  -- Parameters

  -- | Curve characteristic.
  char :: Point f c e q r -> Integer

  -- | Curve cofactor.
  cof :: Point f c e q r -> Integer

  -- | Curve discriminant.
  disc :: Point f c e q r -> q

  -- Points

  -- | Curve point.
  data family Point f c e q r :: *

  -- | Curve point multiplication.
  mul :: r ~ PrimeField p => Point f c e q r -> r -> Point f c e q r
  mul = (. toInt) . mul'
  {-# INLINABLE mul #-}

  -- | Get point from X and Y coordinates.
  point :: q -> q -> Maybe (Point f c e q r)

  -- | Get point from X coordinate.
  pointX :: q -> Maybe (Point f c e q r)

  -- | Get Y coordinate from X coordinate.
  yX :: Point f c e q r -> q -> Maybe q

  -- Coordinates

  -- | Transform from affine coordinates.
  fromA :: Point f 'Affine e q r -> Point f c e q r

  -- | Transform to affine coordinates.
  toA :: Point f c e q r -> Point f 'Affine e q r

-- | Curve forms.
data Form = Binary
          | Edwards
          | Montgomery
          | Weierstrass

-- | Curve coordinates.
data Coordinates = Affine
                 | Jacobian
                 | Projective

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Elliptic curve points are monoids.
instance Curve f c e q r => Monoid (Point f c e q r) where

  mempty = id
  {-# INLINABLE mempty #-}

-- Elliptic curve points are semigroups.
instance Curve f c e q r => Semigroup (Point f c e q r) where

  p <> q = if p == q then dbl p else add p q
  {-# INLINABLE (<>) #-}

-- Elliptic curve points are arbitrary.
instance Curve f c e q r => Arbitrary (Point f c e q r) where

  arbitrary = mul gen <$> arbitrary
  {-# INLINABLE arbitrary #-}

-- Elliptic curve points are random.
instance Curve f c e q r => Random (Point f c e q r) where

  random  = first (mul gen) . random
  {-# INLINABLE random #-}

  randomR = panic "not implemented."
