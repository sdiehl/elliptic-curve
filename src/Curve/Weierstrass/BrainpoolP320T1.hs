module Curve.Weierstrass.BrainpoolP320T1
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , PA
  , PP
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WPCurve(..)
  , WPPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , gA
  , gJ
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP320T1 curve.
data BrainpoolP320T1

-- | Field of points of BrainpoolP320T1 curve.
type Fq = PrimeField 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27

-- | Field of coefficients of BrainpoolP320T1 curve.
type Fr = PrimeField 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311

-- | BrainpoolP320T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP320T1 Fq => WCurve c BrainpoolP320T1 Fq where
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine BrainpoolP320T1 curve point.
type PA = WAPoint BrainpoolP320T1 Fq

-- | Affine BrainpoolP320T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP320T1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BrainpoolP320T1 point.
type PJ = WJPoint BrainpoolP320T1 Fq

-- | Jacobian BrainpoolP320T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP320T1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BrainpoolP320T1 point.
type PP = WPPoint BrainpoolP320T1 Fq

-- | Projective BrainpoolP320T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP320T1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP320T1 curve.
_a :: Fq
_a = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e24
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP320T1 curve.
_b :: Fq
_b = 0xa7f561e038eb1ed560b3d147db782013064c19f27ed27c6780aaf77fb8a547ceb5b4fef422340353
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP320T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP320T1 curve.
_q :: Integer
_q = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27
{-# INLINE _q #-}

-- | Order of BrainpoolP320T1 curve.
_r :: Integer
_r = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP320T1 curve.
_x :: Fq
_x = 0x925be9fb01afc6fb4d3e7d4990010f813408ab106c4f09cb7ee07868cc136fff3357f624a21bed52
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP320T1 curve.
_y :: Fq
_y = 0x63ba3a7a27483ebf6671dbef7abb30ebee084e58a0b077ad42a5a0989d1ee71b1b9bc0455fb0d2c3
{-# INLINE _y #-}

-- | Generator of affine BrainpoolP320T1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of Jacobian BrainpoolP320T1 curve.
gJ :: PJ
gJ = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gJ #-}

-- | Generator of projective BrainpoolP320T1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
