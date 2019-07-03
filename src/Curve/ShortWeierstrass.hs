module Curve.ShortWeierstrass
  -- | Types
  ( P(..)
  , SWCurve(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Short Weierstrass curves @Y^2 = X^3 + aX^2 + b@
class Curve c k => SWCurve c k where
  a :: c -> k -- ^ a
  b :: c -> k -- ^ b

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Short Weierstrass curves are elliptic curves
instance (GaloisField k, SWCurve c k) => Curve c k where

  data instance P c k = A k k -- ^ Affine point
                      | O     -- ^ Infinite point
    deriving (Eq, Show)

  id = O
  {-# INLINE id #-}

  inv O       = O
  inv (A x y) = A x (-y)
  {-# INLINE inv #-}

  add p O          = p
  add O q          = q
  add p@(A x1 y1) (A x2 y2)
    | x1 /= x2     = A x3 y3
    | y1 + y2 == 0 = O
    | otherwise    = double p
    where
      l  = (y1 - y2) / (x1 - x2)
      x3 = l * l - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINE add #-}

  double O       = O
  double (A x y) = A x' y'
    where
      a_ = a (witness :: c)
      l  = (3 * x * x + a_) / (y + y)
      x' = l * l - x - x
      y' = l * (x - x') - y
  {-# INLINE double #-}

  def O       = True
  def (A x y) = y * y == x * x * x + a_ * x + b_
    where
      a_ = a (witness :: c)
      b_ = b (witness :: c)
  {-# INLINE def #-}
