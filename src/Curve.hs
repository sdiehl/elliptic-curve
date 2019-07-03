module Curve
  -- | Types
  ( Curve(..)
  ) where

import Protolude

import GaloisField (GaloisField)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Elliptic curves
class GaloisField k => Curve c k where
  {-# MINIMAL id, inv, add, def #-}

  data family P c k :: *                     -- ^ Elliptic curve point

  id :: P c k                                -- ^ Point identity

  inv :: P c k -> P c k                      -- ^ Point inversion

  add :: P c k -> P c k -> P c k             -- ^ Point addition

  double :: P c k -> P c k                   -- ^ Point doubling
  double = join add
  {-# INLINE double #-}

  scale :: Integral n => n -> P c k -> P c k -- ^ Point scalar multiplication
  scale n p
    | n < 0     = inv (scale (-n) p)
    | n == 0    = id
    | n == 1    = p
    | even n    = p'
    | otherwise = add p p'
    where
      p' = scale (div n 2) (double p)
  {-# INLINE scale #-}

  def :: P c k -> Bool                       -- ^ Point is well-defined

-- | Elliptic curves are semigroups
instance Curve c k => Semigroup (P c k) where
  (<>) = add

-- | Elliptic curves are monoids
instance Curve c k => Monoid (P c k) where
  mempty = id
