module Curve.Weierstrass.BN254A
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

-- | BN254A curve
data BN254A

-- | Field of BN254A curve
type Fp = PrimeField 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001

-- | BN254A curve is a Weierstrass curve
instance WCurve BN254A Fp where
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

-- | Point of BN254A curve
type P = WPoint BN254A Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254A curve
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254A curve
_b :: Fp
_b = 5
{-# INLINE _b #-}

-- | Generator of BN254A curve
_g :: P
_g = A
     1
     0xd45589b158faaf6ab0e4ad38d998e9982e7ff63964ee1460342a592677cccb0
{-# INLINE _g #-}

-- | Cofactor of BN254A curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of BN254A curve
_n :: Integer
_n = 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
{-# INLINE _n #-}

-- | Characteristic of BN254A curve
_p :: Integer
_p = 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001
{-# INLINE _p #-}
