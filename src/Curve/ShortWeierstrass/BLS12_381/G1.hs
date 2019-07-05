module Curve.ShortWeierstrass.BLS12_381.G1
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fp
  , P
  ) where

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
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 4
  {-# INLINE _b #-}
  _g   = A 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
           0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1
  {-# INLINE _g #-}
  _h _ = 0x396c8c005555e1568c00aaab0000aaa
  {-# INLINE _h #-}
  _n _ = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  {-# INLINE _n #-}
  _p _ = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
  {-# INLINE _p #-}

-- | Point of BLS12-381 curve G1
type P = SWPoint G1 Fp
