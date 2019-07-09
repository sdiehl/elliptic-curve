module Curve.BinaryWeierstrass
  -- | Types
  ( BWCurve(..)
  , BWPoint
  , Point(..)
  ) where

import Protolude

import BinaryField (BinaryField)
import Test.Tasty.QuickCheck (Arbitrary(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Binary Weierstrass representation
data BW

-- | Binary Weierstrass points
type BWPoint = Point BW

-- | Binary Weierstrass curves @Y^2 + XY = X^3 + AX^2 + B@
class Curve BW c (BinaryField ib) => BWCurve c ib where
  a_ :: c -> BinaryField ib        -- ^ A
  b_ :: c -> BinaryField ib        -- ^ B
  g_ :: BWPoint c (BinaryField ib) -- ^ generator

-- | Binary Weierstrass curves are arbitrary
instance BWCurve c ib => Arbitrary (Point BW c (BinaryField ib)) where
  arbitrary = return g_

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Binary Weierstrass curves are elliptic curves
instance (KnownNat ib, BWCurve c ib) => Curve BW c (BinaryField ib) where

  data instance Point BW c (BinaryField ib)
    = A (BinaryField ib) (BinaryField ib) -- ^ Affine point
    | O                                   -- ^ Infinite point
    deriving (Eq, Generic, NFData, Show)

  id = O
  {-# INLINE id #-}

  inv O       = O
  inv (A x y) = A x (x + y)
  {-# INLINE inv #-}

  add p O          = p
  add O q          = q
  add p@(A x1 y1) (A x2 y2)
    | xx /= 0      = A x3 y3
    | yy + x2 /= 0 = double p
    | otherwise    = O
    where
      xx = x1 + x2
      yy = y1 + y2
      l  = yy / xx
      x3 = l * (l + 1) + xx + a_ (witness :: c)
      y3 = l * (x1 + x3) + x3 + y1
  {-# INLINE add #-}

  double O       = O
  double (A x y) = A x' y'
    where
      l  = x + y / x
      l' = l + 1
      x' = l * l' + a_ (witness :: c)
      y' = x * x + l' * x'
  {-# INLINE double #-}

  def O       = True
  def (A x y) = ((x + a) * x + y) * x + b + y * y == 0
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE def #-}
