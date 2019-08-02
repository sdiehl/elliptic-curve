module Curve.Weierstrass.BrainpoolP160R1
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
  , pattern A
  , pattern J
  , pattern P
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP160R1 curve.
data BrainpoolP160R1

-- | Field of points of BrainpoolP160R1 curve.
type Fq = PrimeField 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f

-- | Field of coefficients of BrainpoolP160R1 curve.
type Fr = PrimeField 0xe95e4a5f737059dc60df5991d45029409e60fc09

-- | BrainpoolP160R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP160R1 Fq => WCurve c BrainpoolP160R1 Fq where
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

-- | Affine BrainpoolP160R1 curve point.
type PA = WAPoint BrainpoolP160R1 Fq

-- | Affine BrainpoolP160R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP160R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BrainpoolP160R1 point.
type PJ = WJPoint BrainpoolP160R1 Fq

-- | Jacobian BrainpoolP160R1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP160R1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BrainpoolP160R1 point.
type PP = WPPoint BrainpoolP160R1 Fq

-- | Projective BrainpoolP160R1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP160R1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP160R1 curve.
_a :: Fq
_a = 0x340e7be2a280eb74e2be61bada745d97e8f7c300
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP160R1 curve.
_b :: Fq
_b = 0x1e589a8595423412134faa2dbdec95c8d8675e58
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP160R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP160R1 curve.
_q :: Integer
_q = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
{-# INLINE _q #-}

-- | Order of BrainpoolP160R1 curve.
_r :: Integer
_r = 0xe95e4a5f737059dc60df5991d45029409e60fc09
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP160R1 curve.
_x :: Fq
_x = 0xbed5af16ea3f6a4f62938c4631eb5af7bdbcdbc3
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP160R1 curve.
_y :: Fq
_y = 0x1667cb477a1a8ec338f94741669c976316da6321
{-# INLINE _y #-}

-- | Generator of affine BrainpoolP160R1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of Jacobian BrainpoolP160R1 curve.
gJ :: PJ
gJ = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gJ #-}

-- | Generator of projective BrainpoolP160R1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
