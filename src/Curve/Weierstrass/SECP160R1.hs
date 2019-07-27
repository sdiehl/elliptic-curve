module Curve.Weierstrass.SECP160R1
  ( Fp
  , P
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP160R1 curve.
data SECP160R1

-- | Field of SECP160R1 curve.
type Fp = PrimeField 0xffffffffffffffffffffffffffffffff7fffffff

-- | SECP160R1 curve is a Weierstrass curve.
instance WCurve SECP160R1 Fp where
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

-- | Point of SECP160R1 curve.
type P = WPoint SECP160R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP160R1 curve.
_a :: Fp
_a = 0xffffffffffffffffffffffffffffffff7ffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160R1 curve.
_b :: Fp
_b = 0x1c97befc54bd7a8b65acf89f81d4d4adc565fa45
{-# INLINE _b #-}

-- | Generator of SECP160R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP160R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of SECP160R1 curve.
_n :: Integer
_n = 0x100000000000000000001f4c8f927aed3ca752257
{-# INLINE _n #-}

-- | Characteristic of SECP160R1 curve.
_p :: Integer
_p = 0xffffffffffffffffffffffffffffffff7fffffff
{-# INLINE _p #-}

-- | Coordinate @X@ of SECP160R1 curve.
_x :: Fp
_x = 0x4a96b5688ef573284664698968c38bb913cbfc82
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP160R1 curve.
_y :: Fp
_y = 0x23a628553168947d59dcc912042351377ac5fb32
{-# INLINE _y #-}
