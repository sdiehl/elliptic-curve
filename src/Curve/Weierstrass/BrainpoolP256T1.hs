module Curve.Weierstrass.BrainpoolP256T1
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , _a
  , _b
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- BrainpoolP256T1 curve
-------------------------------------------------------------------------------

-- | BrainpoolP256T1 curve.
data BrainpoolP256T1

-- | Field of points of BrainpoolP256T1 curve.
type Fq = PrimeField 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377

-- | Field of coefficients of BrainpoolP256T1 curve.
type Fr = PrimeField 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7

-- | BrainpoolP256T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP256T1 Fq => WCurve c BrainpoolP256T1 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}

-- | Coefficient @A@ of BrainpoolP256T1 curve.
_a :: Fq
_a = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5374
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP256T1 curve.
_b :: Fq
_b = 0x662c61c430d84ea4fe66a7733d0b76b7bf93ebc4af2f49256ae58101fee92b04
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP256T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP256T1 curve.
_q :: Integer
_q = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377
{-# INLINE _q #-}

-- | Order of BrainpoolP256T1 curve.
_r :: Integer
_r = 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BrainpoolP256T1 point.
type AP = WAPoint BrainpoolP256T1 Fq

-- | Affine BrainpoolP256T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP256T1 Fq where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine BrainpoolP256T1 curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine BrainpoolP256T1 curve.
_x :: Fq
_x = 0xa3e8eb3cc1cfe7b7732213b23a656149afa142c47aafbc2b79a191562e1305f4
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine BrainpoolP256T1 curve.
_y :: Fq
_y = 0x2d996c823439c56d7f7b22e14644417e69bcb6de39d027001dabe8f35b25c9be
{-# INLINE _y #-}
