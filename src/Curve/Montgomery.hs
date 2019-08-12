{-# OPTIONS -fno-warn-orphans #-}

module Curve.Montgomery
  ( module Curve
  , module Curve.Montgomery
  , module Group
  , Point(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Coordinates(..), Curve(..), Form(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Montgomery form
-------------------------------------------------------------------------------

-- | Montgomery points.
type MPoint = Point 'Montgomery

-- | Montgomery curves.
class (GaloisField q, GaloisField r, Curve 'Montgomery c e q r) => MCurve c e q r where
  {-# MINIMAL a_, b_, h_, q_, r_, x_, y_ #-}
  a_ :: MPoint c e q r -> q       -- ^ Coefficient @A@.
  b_ :: MPoint c e q r -> q       -- ^ Coefficient @B@.
  h_ :: MPoint c e q r -> Integer -- ^ Curve cofactor.
  q_ :: MPoint c e q r -> Integer -- ^ Curve characteristic.
  r_ :: MPoint c e q r -> Integer -- ^ Curve order.
  x_ :: MPoint c e q r -> q       -- ^ Coordinate @X@.
  y_ :: MPoint c e q r -> q       -- ^ Coordinate @Y@.

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Montgomery affine points.
type MAPoint = MPoint 'Affine

-- | Montgomery affine curves @By^2 = x^3 + Ax^2 + x@.
class MCurve 'Affine e q r => MACurve e q r where
  {-# MINIMAL gA_ #-}
  gA_ :: MAPoint e q r -- ^ Curve generator.

-- Montgomery affine curves are elliptic curves.
instance MACurve e q r => Curve 'Montgomery 'Affine e q r where

  data instance Point 'Montgomery 'Affine e q r = A q q -- ^ Affine point.
                                                | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = q_
  {-# INLINABLE char #-}

  cof = h_
  {-# INLINABLE cof #-}

  disc _ = b * (a * a - 4)
    where
      a = a_ (witness :: MAPoint e q r)
      b = b_ (witness :: MAPoint e q r)
  {-# INLINABLE disc #-}

  fromA = identity
  {-# INLINABLE fromA #-}

  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINABLE point #-}

  pointX x = A x <$> yX (witness :: MAPoint e q r) x
  {-# INLINABLE pointX #-}

  toA = identity
  {-# INLINABLE toA #-}

  yX _ x = sr ((((x + a) * x) + 1) * x / b)
    where
      a = a_ (witness :: MAPoint e q r)
      b = b_ (witness :: MAPoint e q r)
  {-# INLINABLE yX #-}

-- Montgomery affine points are groups.
instance MACurve e q r => Group (MAPoint e q r) where

  add p  O      = p
  add O q       = q
  add (A x1 y1) (A x2 y2)
    | x1 == x2  = O
    | otherwise = A x3 y3
    where
      a  = a_ (witness :: MAPoint e q r)
      b  = b_ (witness :: MAPoint e q r)
      l  = (y2 - y1) / (x2 - x1)
      x3 = b * l * l - a - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINABLE add #-}

  dbl O         = O
  dbl (A x y)
    | y == 0    = O
    | otherwise = A x' y'
    where
      a  = a_ (witness :: MAPoint e q r)
      b  = b_ (witness :: MAPoint e q r)
      l  = (x * (3 * x + 2 * a) + 1) / (2 * b * y)
      x' = b * l * l - a - 2 * x
      y' = l * (x - x') - y
  {-# INLINABLE dbl #-}

  def O       = True
  def (A x y) = b * y * y == (((x + a) * x) + 1) * x
    where
      a = a_ (witness :: MAPoint e q r)
      b = b_ (witness :: MAPoint e q r)
  {-# INLINABLE def #-}

  gen = gA_
  {-# INLINABLE gen #-}

  id = O
  {-# INLINABLE id #-}

  inv O       = O
  inv (A x y) = A x (-y)
  {-# INLINABLE inv #-}

  order = r_
  {-# INLINABLE order #-}

-- Montgomery affine points are pretty.
instance MACurve e q r => Pretty (MAPoint e q r) where

  pretty (A x y) = pretty (x, y)
  pretty O       = "O"
