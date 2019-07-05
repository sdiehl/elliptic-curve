module Curve.BinaryWeierstrass
  -- | Types
  ( BWCurve(..)
  , BWPoint
  , F2
  , Fm
  , Point(..)
  ) where

import Protolude

import PrimeField (PrimeField)
import ExtensionField (ExtensionField, IrreducibleMonic)

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
  _a :: (c, im) -> Fm im   -- ^ A
  _b :: (c, im) -> Fm im   -- ^ B
  _f :: (c, im) -> im      -- ^ polynomial
  _g :: BWPoint c (Fm im)  -- ^ generator
  _h :: (c, im) -> Integer -- ^ cofactor
  _r :: (c, im) -> Integer -- ^ order

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
      a  = _a (witness :: (c, im))
      xx = x1 + x2
      yy = y1 + y2
      x3 = a + xx
      y3 = a * yy / xx + x3
  {-# INLINE add #-}

  double O       = O
  double (A x y) = A x' y'
    where
      x' = _a (witness :: (c, im))
      y' = x * x + x' * (x + y / x + 1)
  {-# INLINE double #-}

  def O       = True
  def (A x y) = y * (x + y) + xx * (x + a) + b == 0
    where
      a  = _a (witness :: (c, im))
      b  = _b (witness :: (c, im))
      xx = x * x
  {-# INLINE def #-}
