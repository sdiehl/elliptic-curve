module Curve.ShortWeierstrass.BN512
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

-- | BN512 curve
data BN512

-- | Field of BN512 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef455146cf1eacbe98b8e48c65deab236fe1916a55ce5f4c6467b4eb280922adef33

-- | BN512 curve is a short Weierstrass curve
instance SWCurve BN512 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of BN512 curve
type P = SWPoint BN512 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN512 curve
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN512 curve
_b :: Fp
_b = 3
{-# INLINE _b #-}

-- | Generator of BN512 curve
_g :: P
_g = A
     1
     2
{-# INLINE _g #-}

-- | Cofactor of BN512 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of BN512 curve
_n :: Integer
_n = 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef445146cf1eacbe98b8e48c65deab2679a34a10313e04f9a2b406a64a5f519a09ed
{-# INLINE _n #-}

-- | Characteristic of BN512 curve
_p :: Integer
_p = 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef455146cf1eacbe98b8e48c65deab236fe1916a55ce5f4c6467b4eb280922adef33
{-# INLINE _p #-}
