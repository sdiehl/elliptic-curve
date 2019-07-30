module Curve.Weierstrass.BrainpoolP512T1
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
  , _h
  , _q
  , _r
  , gA
  , xA
  , yA
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- BrainpoolP512T1 curve
-------------------------------------------------------------------------------

-- | BrainpoolP512T1 curve.
data BrainpoolP512T1

-- | Field of points of BrainpoolP512T1 curve.
type Fq = PrimeField 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3

-- | Field of coefficients of BrainpoolP512T1 curve.
type Fr = PrimeField 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069

-- | BrainpoolP512T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP512T1 Fq => WCurve c BrainpoolP512T1 Fq where
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

-- | Coefficient @A@ of BrainpoolP512T1 curve.
_a :: Fq
_a = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f0
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP512T1 curve.
_b :: Fq
_b = 0x7cbbbcf9441cfab76e1890e46884eae321f70c0bcb4981527897504bec3e36a62bcdfa2304976540f6450085f2dae145c22553b465763689180ea2571867423e
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP512T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP512T1 curve.
_q :: Integer
_q = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3
{-# INLINE _q #-}

-- | Order of BrainpoolP512T1 curve.
_r :: Integer
_r = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BrainpoolP512T1 point.
type AP = WAPoint BrainpoolP512T1 Fq

-- | Affine BrainpoolP512T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP512T1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine BrainpoolP512T1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine BrainpoolP512T1 curve.
xA :: Fq
xA = 0x640ece5c12788717b9c1ba06cbc2a6feba85842458c56dde9db1758d39c0313d82ba51735cdb3ea499aa77a7d6943a64f7a3f25fe26f06b51baa2696fa9035da
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine BrainpoolP512T1 curve.
yA :: Fq
yA = 0x5b534bd595f5af0fa2c892376c84ace1bb4e3019b71634c01131159cae03cee9d9932184beef216bd71df2dadf86a627306ecff96dbb8bace198b61e00f8b332
{-# INLINE yA #-}
