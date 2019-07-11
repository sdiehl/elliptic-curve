module Curve.Weierstrass.BrainpoolP512T1
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

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Brainpool-P512T1 curve
data BrainpoolP512T1

-- | Field of Brainpool-P512T1 curve
type Fp = PrimeField 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3

-- | Brainpool-P512T1 curve is a Weierstrass curve
instance WCurve BrainpoolP512T1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Brainpool-P512T1 curve
type P = WPoint BrainpoolP512T1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Brainpool-P512T1 curve
_a :: Fp
_a = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f0
{-# INLINE _a #-}

-- | Coefficient @B@ of Brainpool-P512T1 curve
_b :: Fp
_b = 0x7cbbbcf9441cfab76e1890e46884eae321f70c0bcb4981527897504bec3e36a62bcdfa2304976540f6450085f2dae145c22553b465763689180ea2571867423e
{-# INLINE _b #-}

-- | Generator of Brainpool-P512T1 curve
_g :: P
_g = A
     0x640ece5c12788717b9c1ba06cbc2a6feba85842458c56dde9db1758d39c0313d82ba51735cdb3ea499aa77a7d6943a64f7a3f25fe26f06b51baa2696fa9035da
     0x5b534bd595f5af0fa2c892376c84ace1bb4e3019b71634c01131159cae03cee9d9932184beef216bd71df2dadf86a627306ecff96dbb8bace198b61e00f8b332
{-# INLINE _g #-}

-- | Cofactor of Brainpool-P512T1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of Brainpool-P512T1 curve
_n :: Integer
_n = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069
{-# INLINE _n #-}

-- | Characteristic of Brainpool-P512T1 curve
_p :: Integer
_p = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3
{-# INLINE _p #-}
