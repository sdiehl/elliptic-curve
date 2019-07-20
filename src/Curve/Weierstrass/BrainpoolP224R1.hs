module Curve.Weierstrass.BrainpoolP224R1
  ( Fp
  , P
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Brainpool-P224R1 curve
data BrainpoolP224R1

-- | Field of Brainpool-P224R1 curve
type Fp = PrimeField 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff

-- | Brainpool-P224R1 curve is a Weierstrass curve
instance WCurve BrainpoolP224R1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Brainpool-P224R1 curve
type P = WPoint BrainpoolP224R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Brainpool-P224R1 curve
_a :: Fp
_a = 0x68a5e62ca9ce6c1c299803a6c1530b514e182ad8b0042a59cad29f43
{-# INLINE _a #-}

-- | Coefficient @B@ of Brainpool-P224R1 curve
_b :: Fp
_b = 0x2580f63ccfe44138870713b1a92369e33e2135d266dbb372386c400b
{-# INLINE _b #-}

-- | Generator of Brainpool-P224R1 curve
_g :: P
_g = A
     0xd9029ad2c7e5cf4340823b2a87dc68c9e4ce3174c1e6efdee12c07d
     0x58aa56f772c0726f24c6b89e4ecdac24354b9e99caa3f6d3761402cd
{-# INLINE _g #-}

-- | Cofactor of Brainpool-P224R1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of Brainpool-P224R1 curve
_n :: Integer
_n = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
{-# INLINE _n #-}

-- | Characteristic of Brainpool-P224R1 curve
_p :: Integer
_p = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
{-# INLINE _p #-}
