module Curve
  ( Curve(..)
  ) where

import Protolude

import GaloisField (GaloisField)

-- | Elliptic curves
class GaloisField f => Curve c f where
  {-# MINIMAL id, inv, add, def #-}

  data family P c f :: *                     -- ^ Elliptic curve point

  id :: P c f                                -- ^ Point identity

  inv :: P c f -> P c f                      -- ^ Point inversion

  add :: P c f -> P c f -> P c f             -- ^ Point addition

  double :: P c f -> P c f                   -- ^ Point doubling
  double = join add
  {-# INLINE double #-}

  scale :: Integral n => n -> P c f -> P c f -- ^ Point scalar multiplication
  scale n p
    | n < 0     = inv (scale (-n) p)
    | n == 0    = mempty
    | n == 1    = p
    | even n    = p'
    | otherwise = p <> p'
    where
      p' = scale (div n 2) (double p)
  {-# INLINE scale #-}

  def :: P c f -> Bool                       -- ^ Point is well-defined

-- | Elliptic curves are semigroups
instance Curve c f => Semigroup (P c f) where
  (<>) = add

-- | Elliptic curves are monoids
instance Curve c f => Monoid (P c f) where
  mempty = id
