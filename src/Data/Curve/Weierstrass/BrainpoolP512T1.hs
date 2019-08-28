module Data.Curve.Weierstrass.BrainpoolP512T1
  ( module Data.Curve.Weierstrass
  , module Data.Curve.Weierstrass.BrainpoolP512T1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP512T1 curve.
data BrainpoolP512T1

-- | Field of points of BrainpoolP512T1 curve.
type Fq = Prime 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3

-- | Field of coefficients of BrainpoolP512T1 curve.
type Fr = Prime 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069

-- | BrainpoolP512T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP512T1 Fq Fr => WCurve c BrainpoolP512T1 Fq Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  b_ = const _b
  {-# INLINABLE b_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  q_ = const _q
  {-# INLINABLE q_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine BrainpoolP512T1 curve point.
type PA = WAPoint BrainpoolP512T1 Fq Fr

-- | Affine BrainpoolP512T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP512T1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP512T1 point.
type PJ = WJPoint BrainpoolP512T1 Fq Fr

-- | Jacobian BrainpoolP512T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP512T1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP512T1 point.
type PP = WPPoint BrainpoolP512T1 Fq Fr

-- | Projective BrainpoolP512T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP512T1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP512T1 curve.
_a :: Fq
_a = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP512T1 curve.
_b :: Fq
_b = 0x7cbbbcf9441cfab76e1890e46884eae321f70c0bcb4981527897504bec3e36a62bcdfa2304976540f6450085f2dae145c22553b465763689180ea2571867423e
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP512T1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP512T1 curve.
_q :: Natural
_q = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3
{-# INLINABLE _q #-}

-- | Order of BrainpoolP512T1 curve.
_r :: Natural
_r = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP512T1 curve.
_x :: Fq
_x = 0x640ece5c12788717b9c1ba06cbc2a6feba85842458c56dde9db1758d39c0313d82ba51735cdb3ea499aa77a7d6943a64f7a3f25fe26f06b51baa2696fa9035da
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP512T1 curve.
_y :: Fq
_y = 0x5b534bd595f5af0fa2c892376c84ace1bb4e3019b71634c01131159cae03cee9d9932184beef216bd71df2dadf86a627306ecff96dbb8bace198b61e00f8b332
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP512T1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP512T1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP512T1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
