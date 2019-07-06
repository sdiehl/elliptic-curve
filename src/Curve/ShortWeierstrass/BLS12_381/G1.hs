module Curve.ShortWeierstrass.BLS12_381.G1
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

-- | BLS12-381 curve G1
data G1

-- | Field of BLS12-381 curve G1
type Fp = PrimeField 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

-- | BLS12-381 curve G1 is a short Weierstrass curve
instance SWCurve G1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of BLS12-381 curve G1
type P = SWPoint G1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BLS12-381 curve G1
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BLS12-381 curve G1
_b :: Fp
_b = 4
{-# INLINE _b #-}

-- | Generator of BLS12-381 curve G1
_g :: P
_g = A 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
       0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1
{-# INLINE _g #-}

-- | Cofactor of BLS12-381 curve G1
_h :: Integer
_h = 0x396c8c005555e1568c00aaab0000aaa
{-# INLINE _h #-}

-- | Order of BLS12-381 curve G1
_n :: Integer
_n = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINE _n #-}

-- | Characteristic of BLS12-381 curve G1
_p :: Integer
_p = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
{-# INLINE _p #-}
