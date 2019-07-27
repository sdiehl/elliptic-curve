module Curve.Weierstrass.SECP192R1
  ( Curve(..)
  , Fp
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
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

import Curve (Curve(..), Group(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP192R1 curve.
data SECP192R1

-- | Field of SECP192R1 curve.
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffeffffffffffffffff

-- | SECP192R1 curve is a Weierstrass curve.
instance WCurve SECP192R1 Fp where
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of SECP192R1 curve.
type P = WPoint SECP192R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP192R1 curve.
_a :: Fp
_a = 0xfffffffffffffffffffffffffffffffefffffffffffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP192R1 curve.
_b :: Fp
_b = 0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
{-# INLINE _b #-}

-- | Generator of SECP192R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP192R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of SECP192R1 curve.
_n :: Integer
_n = 0xffffffffffffffffffffffff99def836146bc9b1b4d22831
{-# INLINE _n #-}

-- | Characteristic of SECP192R1 curve.
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffeffffffffffffffff
{-# INLINE _p #-}

-- | Coordinate @X@ of SECP192R1 curve.
_x :: Fp
_x = 0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP192R1 curve.
_y :: Fp
_y = 0x7192b95ffc8da78631011ed6b24cdd573f977a11e794811
{-# INLINE _y #-}
