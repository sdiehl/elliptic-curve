module Curve.Weierstrass.BrainpoolP320R1
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

-- | BrainpoolP320R1 curve.
data BrainpoolP320R1

-- | Field of points of BrainpoolP320R1 curve.
type Fq = PrimeField 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27

-- | Field of coefficients of BrainpoolP320R1 curve.
type Fr = PrimeField 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311

-- | BrainpoolP320R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP320R1 Fq => WCurve c BrainpoolP320R1 Fq where
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

-- | Affine BrainpoolP320R1 curve point.
type PA = WAPoint BrainpoolP320R1 Fq

-- | Affine BrainpoolP320R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP320R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BrainpoolP320R1 point.
type PJ = WJPoint BrainpoolP320R1 Fq

-- | Jacobian BrainpoolP320R1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP320R1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BrainpoolP320R1 point.
type PP = WPPoint BrainpoolP320R1 Fq

-- | Projective BrainpoolP320R1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP320R1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP320R1 curve.
_a :: Fq
_a = 0x3ee30b568fbab0f883ccebd46d3f3bb8a2a73513f5eb79da66190eb085ffa9f492f375a97d860eb4
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP320R1 curve.
_b :: Fq
_b = 0x520883949dfdbc42d3ad198640688a6fe13f41349554b49acc31dccd884539816f5eb4ac8fb1f1a6
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP320R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP320R1 curve.
_q :: Integer
_q = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27
{-# INLINE _q #-}

-- | Order of BrainpoolP320R1 curve.
_r :: Integer
_r = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP320R1 curve.
_x :: Fq
_x = 0x43bd7e9afb53d8b85289bcc48ee5bfe6f20137d10a087eb6e7871e2a10a599c710af8d0d39e20611
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP320R1 curve.
_y :: Fq
_y = 0x14fdd05545ec1cc8ab4093247f77275e0743ffed117182eaa9c77877aaac6ac7d35245d1692e8ee1
{-# INLINE _y #-}

-- | Generator of affine BrainpoolP320R1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of Jacobian BrainpoolP320R1 curve.
gJ :: PJ
gJ = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gJ #-}

-- | Generator of projective BrainpoolP320R1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
