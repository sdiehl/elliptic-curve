{-# OPTIONS -fno-warn-orphans #-}

module Curve.Edwards
  ( module Curve
  , module Curve.Edwards
  , module Group
  , Point(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Coordinates(..), Curve(..), Form(..), PrimeField')
import Group (Group(..))

-------------------------------------------------------------------------------
-- Edwards form
-------------------------------------------------------------------------------

-- | Edwards points.
type EPoint = Point 'Edwards

-- | Edwards curves.
class (GaloisField q, GaloisField r, PrimeField' r, Curve 'Edwards c e q r)
  => ECurve c e q r where
  {-# MINIMAL a_, d_, h_, q_, r_, x_, y_ #-}
  a_ :: EPoint c e q r -> q       -- ^ Coefficient @A@.
  d_ :: EPoint c e q r -> q       -- ^ Coefficient @D@.
  h_ :: EPoint c e q r -> Integer -- ^ Curve cofactor.
  q_ :: EPoint c e q r -> Integer -- ^ Curve characteristic.
  r_ :: EPoint c e q r -> Integer -- ^ Curve order.
  x_ :: EPoint c e q r -> q       -- ^ Coordinate @X@.
  y_ :: EPoint c e q r -> q       -- ^ Coordinate @Y@.

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Edwards affine curves.
type EAPoint = EPoint 'Affine

-- | Edwards affine curves @Ax^2 + y^2 = 1 + Dx^2y^2@.
class ECurve 'Affine e q r => EACurve e q r where
  {-# MINIMAL gA_ #-}
  gA_ :: EAPoint e q r -- ^ Curve generator.

-- Edwards affine curves are elliptic curves.
instance EACurve e q r => Curve 'Edwards 'Affine e q r where

  data instance Point 'Edwards 'Affine e q r = A q q -- ^ Affine point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = q_
  {-# INLINABLE char #-}

  cof = h_
  {-# INLINABLE cof #-}

  disc _ = d * (1 - d)
    where
      d = d_ (witness :: EAPoint e q r)
  {-# INLINABLE disc #-}

  fromA = identity
  {-# INLINABLE fromA #-}

  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINABLE point #-}

  pointX x = A x <$> yX (witness :: EAPoint e q r) x
  {-# INLINABLE pointX #-}

  toA = identity
  {-# INLINABLE toA #-}

  yX _ x = sr ((1 - a * xx) / (1 - d * xx))
    where
      a  = a_ (witness :: EAPoint e q r)
      d  = d_ (witness :: EAPoint e q r)
      xx = x * x
  {-# INLINABLE yX #-}

-- Edwards affine points are groups.
instance EACurve e q r => Group (EAPoint e q r) where

  add (A x1 y1) (A x2 y2) = A x3 y3
    where
      a    = a_ (witness :: EAPoint e q r)
      d    = d_ (witness :: EAPoint e q r)
      x1x2 = x1 * x2
      y1y2 = y1 * y2
      x1y2 = x1 * y2
      x2y1 = x2 * y1
      dxy  = d * x1x2 * y1y2
      x3   = (x1y2 + x2y1) / (1 + dxy)
      y3   = (y1y2 - a * x1x2) / (1 - dxy)
  {-# INLINABLE add #-}

  dbl = join add
  {-# INLINABLE dbl #-}

  def (A x y) = a * xx + yy == 1 + d * xx * yy
    where
      a  = a_ (witness :: EAPoint e q r)
      d  = d_ (witness :: EAPoint e q r)
      xx = x * x
      yy = y * y
  {-# INLINABLE def #-}

  gen = gA_
  {-# INLINABLE gen #-}

  id = A 0 1
  {-# INLINABLE id #-}

  inv (A x y) = A (-x) y
  {-# INLINABLE inv #-}

  order = r_
  {-# INLINABLE order #-}

-- Edwards affine points are pretty.
instance EACurve e q r => Pretty (EAPoint e q r) where

  pretty (A x y) = pretty (x, y)

-------------------------------------------------------------------------------
-- Projective coordinates
-------------------------------------------------------------------------------

-- | Edwards projective curves.
type EPPoint = EPoint 'Projective

-- | Edwards projective curves @Ax^2z^2 + y^2z^2 = z^4 + Dx^2y^2@.
class ECurve 'Projective e q r => EPCurve e q r where
  {-# MINIMAL gP_ #-}
  gP_ :: EPPoint e q r -- ^ Curve generator.

-- Edwards projective curves are elliptic curves.
instance EPCurve e q r => Curve 'Edwards 'Projective e q r where

  data instance Point 'Edwards 'Projective e q r = P q q q -- ^ Projective point.
    deriving (Generic, NFData, Read, Show)

  char = q_
  {-# INLINABLE char #-}

  cof = h_
  {-# INLINABLE cof #-}

  disc _ = d * (1 - d)
    where
      d = d_ (witness :: EPPoint e q r)
  {-# INLINABLE disc #-}

  fromA (A x y) = P x y 1
  {-# INLINABLE fromA #-}

  point x y = let p = P x y 1 in if def p then Just p else Nothing
  {-# INLINABLE point #-}

  pointX x = flip (P x) 1 <$> yX (witness :: EPPoint e q r) x
  {-# INLINABLE pointX #-}

  toA (P x y z) = A (x / z) (y / z)
  {-# INLINABLE toA #-}

  yX _ x = sr ((1 - a * xx) / (1 - d * xx))
    where
      a  = a_ (witness :: EPPoint e q r)
      d  = d_ (witness :: EPPoint e q r)
      xx = x * x
  {-# INLINABLE yX #-}

-- Edwards projective points are groups.
instance EPCurve e q r => Group (EPPoint e q r) where

  -- Addition formula add-2008-bbjlp
  add (P x1 y1 z1) (P x2 y2 z2) = P x3 y3 z3
    where
      a' = a_ (witness :: EPPoint e q r)
      d' = d_ (witness :: EPPoint e q r)
      a  = z1 * z2
      b  = a * a
      c  = x1 * x2
      d  = y1 * y2
      e  = d' * c * d
      f  = b - e
      g  = b + e
      x3 = a * f * ((x1 + y1) * (x2 + y2) - c - d)
      y3 = a * g * (d - a' * c)
      z3 = f * g
  {-# INLINABLE add #-}

  -- Doubling formula dbl-2008-bbjlp
  dbl (P x1 y1 z1) = P x3 y3 z3
    where
      a  = a_ (witness :: EPPoint e q r)
      xy = x1 + y1
      b  = xy * xy
      c  = x1 * x1
      d  = y1 * y1
      e  = a * c
      f  = e + d
      h  = z1 * z1
      j  = f - 2 * h
      x3 = (b - c - d) * j
      y3 = f * (e - d)
      z3 = f * j
  {-# INLINABLE dbl #-}

  def (P x y z) = (a * xx + yy - zz) * zz == d * xx * yy
    where
      a  = a_ (witness :: EPPoint e q r)
      d  = d_ (witness :: EPPoint e q r)
      xx = x * x
      yy = y * y
      zz = z * z
  {-# INLINABLE def #-}

  gen = gP_
  {-# INLINABLE gen #-}

  id = P 0 1 1
  {-# INLINABLE id #-}

  inv (P x y z) = P (-x) y z
  {-# INLINABLE inv #-}

  order = r_
  {-# INLINABLE order #-}

-- Edwards projective points are equatable.
instance EPCurve e q r => Eq (EPPoint e q r) where

  P x1 y1 z1 == P x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * z2 == x2 * z1 && y1 * z2 == y2 * z1

-- Edwards projective points are pretty.
instance EPCurve e q r => Pretty (EPPoint e q r) where

  pretty (P x y z) = pretty (x, y, z)
