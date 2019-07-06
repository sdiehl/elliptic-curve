module Curve.BinaryWeierstrass
  -- | Types
  ( BWCurve(..)
  , BWPoint
  , F2
  , Fm
  , Point(..)
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic)
import PrimeField (PrimeField)
import Test.Tasty.QuickCheck (Arbitrary(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Binary field
type F2 = PrimeField 2

-- | Extension field over binary field
type Fm = ExtensionField F2

-- | Binary Weierstrass representation
data BW

-- | Binary Weierstrass points
type BWPoint = Point BW

-- | Binary Weierstrass curves @Y^2 + XY = X^3 + AX^2 + B@
class (IrreducibleMonic F2 im, Curve BW c (Fm im)) => BWCurve c im where
  a_ :: (c, im) -> Fm im  -- ^ A
  b_ :: (c, im) -> Fm im  -- ^ B
  g_ :: BWPoint c (Fm im) -- ^ generator

-- | Binary Weierstrass curves are arbitrary
instance BWCurve c im => Arbitrary (Point BW c (Fm im)) where
  arbitrary = return g_

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Binary Weierstrass curves are elliptic curves
instance (IrreducibleMonic F2 im, BWCurve c im) => Curve BW c (Fm im) where

  data instance Point BW c (Fm im) = A (Fm im) (Fm im) -- ^ Affine point
                                   | O                 -- ^ Infinite point
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
      x3 = l * (l + 1) + xx + a_ (witness :: (c, im))
      y3 = l * (x1 + x3) + x3 + y1
  {-# INLINE add #-}

  double O       = O
  double (A x y) = A x' y'
    where
      l  = x + y / x
      l' = l + 1
      x' = l * l' + a_ (witness :: (c, im))
      y' = x * x + l' * x'
  {-# INLINE double #-}

  def O       = True
  def (A x y) = ((x + a) * x + y) * x + b + y * y == 0
    where
      a = a_ (witness :: (c, im))
      b = b_ (witness :: (c, im))
  {-# INLINE def #-}
