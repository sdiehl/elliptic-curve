module Curve.Weierstrass.BrainpoolP192R1
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

-- | BrainpoolP192R1 curve.
data BrainpoolP192R1

-- | Field of points of BrainpoolP192R1 curve.
type Fq = PrimeField 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297

-- | Field of coefficients of BrainpoolP192R1 curve.
type Fr = PrimeField 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1

-- | BrainpoolP192R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP192R1 Fq => WCurve c BrainpoolP192R1 Fq where
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

-- | Affine BrainpoolP192R1 curve point.
type PA = WAPoint BrainpoolP192R1 Fq

-- | Affine BrainpoolP192R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP192R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BrainpoolP192R1 point.
type PJ = WJPoint BrainpoolP192R1 Fq

-- | Jacobian BrainpoolP192R1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP192R1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BrainpoolP192R1 point.
type PP = WPPoint BrainpoolP192R1 Fq

-- | Projective BrainpoolP192R1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP192R1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP192R1 curve.
_a :: Fq
_a = 0x6a91174076b1e0e19c39c031fe8685c1cae040e5c69a28ef
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP192R1 curve.
_b :: Fq
_b = 0x469a28ef7c28cca3dc721d044f4496bcca7ef4146fbf25c9
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP192R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP192R1 curve.
_q :: Integer
_q = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
{-# INLINE _q #-}

-- | Order of BrainpoolP192R1 curve.
_r :: Integer
_r = 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP192R1 curve.
_x :: Fq
_x = 0xc0a0647eaab6a48753b033c56cb0f0900a2f5c4853375fd6
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP192R1 curve.
_y :: Fq
_y = 0x14b690866abd5bb88b5f4828c1490002e6773fa2fa299b8f
{-# INLINE _y #-}

-- | Generator of affine BrainpoolP192R1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of Jacobian BrainpoolP192R1 curve.
gJ :: PJ
gJ = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gJ #-}

-- | Generator of projective BrainpoolP192R1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
