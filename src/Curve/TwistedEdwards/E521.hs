module Curve.TwistedEdwards.E521
  -- | Types
  ( Fp
  , P
  -- | Parameters
  , _a
  , _d
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.TwistedEdwards (Point(..), TECurve(..), TEPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | E-521 curve
data E521

-- | Field of E-521 curve
type Fp = PrimeField 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | E-521 curve is a twisted Edwards curve
instance TECurve E521 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of E-521 curve
type P = TEPoint E521 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of E-521 curve
_a :: Fp
_a = 1
{-# INLINE _a #-}

-- | Coefficient @D@ of E-521 curve
_d :: Fp
_d = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa4331
{-# INLINE _d #-}

-- | Generator of E-521 curve
_g :: P
_g = A
     0x752cb45c48648b189df90cb2296b2878a3bfd9f42fc6c818ec8bf3c9c0c6203913f6ecc5ccc72434b1ae949d568fc99c6059d0fb13364838aa302a940a2f19ba6c
     0xc
{-# INLINE _g #-}

-- | Cofactor of E-521 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of E-521 curve
_n :: Integer
_n = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd15b6c64746fc85f736b8af5e7ec53f04fbd8c4569a8f1f4540ea2435f5180d6b
{-# INLINE _n #-}

-- | Characteristic of E-521 curve
_p :: Integer
_p = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINE _p #-}
