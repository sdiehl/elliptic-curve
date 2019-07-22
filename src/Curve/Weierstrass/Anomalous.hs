module Curve.Weierstrass.Anomalous
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

-- | Anomalous curve
data Anomalous

-- | Field of Anomalous curve
type Fp = PrimeField 0xb0000000000000000000000953000000000000000000001f9d7

-- | Anomalous curve is a Weierstrass curve
instance WCurve Anomalous Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Point of Anomalous curve
type P = WPoint Anomalous Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Anomalous curve
_a :: Fp
_a = 0x98d0fac687d6343eb1a1f595283eb1a1f58d0fac687d635f5e4
{-# INLINE _a #-}

-- | Coefficient @B@ of Anomalous curve
_b :: Fp
_b = 0x4a1f58d0fac687d6343eb1a5e2d6343eb1a1f58d0fac688ab3f
{-# INLINE _b #-}

-- | Generator of Anomalous curve
_g :: P
_g = A
     0x101efb35fd1963c4871a2d17edaafa7e249807f58f8705126c6
     0x22389a3954375834304ba1d509a97de6c07148ea7f5951b20e7
{-# INLINE _g #-}

-- | Cofactor of Anomalous curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of Anomalous curve
_n :: Integer
_n = 0xb0000000000000000000000953000000000000000000001f9d7
{-# INLINE _n #-}

-- | Characteristic of Anomalous curve
_p :: Integer
_p = 0xb0000000000000000000000953000000000000000000001f9d7
{-# INLINE _p #-}
