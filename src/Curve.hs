module Curve
  ( Curve(..)
  , Group(..)
  ) where

import Protolude

import GaloisField (GaloisField)

import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Elliptic curves.
class (GaloisField k, Group (Point f c k)) => Curve f c k where
  {-# MINIMAL char, cof, disc, point, pointX, yX #-}

  -- | Curve point.
  data family Point f c k :: *

  -- | Curve characteristic.
  char :: Point f c k -> Integer

  -- | Curve cofactor.
  cof :: Point f c k -> Integer

  -- | Curve discriminant.
  disc :: Point f c k -> k

  -- | Point from X and Y coordinates.
  point :: k -> k -> Maybe (Point f c k)

  -- | Point from X coordinate.
  pointX :: k -> Maybe (Point f c k)

  -- | Y coordinate from X coordinate.
  yX :: Point f c k -> k -> Maybe k
