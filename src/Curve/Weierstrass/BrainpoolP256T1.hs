module Curve.Weierstrass.BrainpoolP256T1
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

-- | BrainpoolP256T1 curve
data BrainpoolP256T1

-- | Field of BrainpoolP256T1 curve
type Fp = PrimeField 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377

-- | BrainpoolP256T1 curve is a Weierstrass curve
instance WCurve BrainpoolP256T1 Fp where
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

-- | Point of BrainpoolP256T1 curve
type P = WPoint BrainpoolP256T1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP256T1 curve
_a :: Fp
_a = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5374
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP256T1 curve
_b :: Fp
_b = 0x662c61c430d84ea4fe66a7733d0b76b7bf93ebc4af2f49256ae58101fee92b04
{-# INLINE _b #-}

-- | Generator of BrainpoolP256T1 curve
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BrainpoolP256T1 curve
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of BrainpoolP256T1 curve
_n :: Integer
_n = 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7
{-# INLINE _n #-}

-- | Characteristic of BrainpoolP256T1 curve
_p :: Integer
_p = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377
{-# INLINE _p #-}

-- | Coordinate @X@ of BrainpoolP256T1 curve
_x :: Fp
_x = 0xa3e8eb3cc1cfe7b7732213b23a656149afa142c47aafbc2b79a191562e1305f4
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP256T1 curve
_y :: Fp
_y = 0x2d996c823439c56d7f7b22e14644417e69bcb6de39d027001dabe8f35b25c9be
{-# INLINE _y #-}
