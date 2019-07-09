module Curve.ShortWeierstrass.SECP160R2
  -- | Types
  ( Fp
  , P
  -- | Parameters
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP160R2 curve
data SECP160R2

-- | Field of SECP160R2 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffeffffac73

-- | SECP160R2 curve is a short Weierstrass curve
instance SWCurve SECP160R2 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECP160R2 curve
type P = SWPoint SECP160R2 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP160R2 curve
_a :: Fp
_a = 0xfffffffffffffffffffffffffffffffeffffac70
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160R2 curve
_b :: Fp
_b = 0xb4e134d3fb59eb8bab57274904664d5af50388ba
{-# INLINE _b #-}

-- | Generator of SECP160R2 curve
_g :: P
_g = A
     0x0052dcb034293a117e1f4ff11b30f7199d3144ce6d
     0x00feaffef2e331f296e071fa0df9982cfea7d43f2e
{-# INLINE _g #-}

-- | Cofactor of SECP160R2 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of SECP160R2 curve
_n :: Integer
_n = 0x0100000000000000000000351ee786a818f3a1a16b
{-# INLINE _n #-}

-- | Characteristic of SECP160R2 curve
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffeffffac73
{-# INLINE _p #-}
