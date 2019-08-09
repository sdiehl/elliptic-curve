module Curve.Weierstrass.BrainpoolP256T1
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , PA
  , PJ
  , PP
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WJCurve(..)
  , WJPoint
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

-- | BrainpoolP256T1 curve.
data BrainpoolP256T1

-- | Field of points of BrainpoolP256T1 curve.
type Fq = PrimeField 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377

-- | Field of coefficients of BrainpoolP256T1 curve.
type Fr = PrimeField 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7

-- | BrainpoolP256T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP256T1 Fq Fr => WCurve c BrainpoolP256T1 Fq Fr where
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

-- | Affine BrainpoolP256T1 curve point.
type PA = WAPoint BrainpoolP256T1 Fq Fr

-- | Affine BrainpoolP256T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP256T1 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BrainpoolP256T1 point.
type PJ = WJPoint BrainpoolP256T1 Fq Fr

-- | Jacobian BrainpoolP256T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP256T1 Fq Fr where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BrainpoolP256T1 point.
type PP = WPPoint BrainpoolP256T1 Fq Fr

-- | Projective BrainpoolP256T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP256T1 Fq Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP256T1 curve.
_a :: Fq
_a = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5374
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP256T1 curve.
_b :: Fq
_b = 0x662c61c430d84ea4fe66a7733d0b76b7bf93ebc4af2f49256ae58101fee92b04
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP256T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP256T1 curve.
_q :: Integer
_q = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377
{-# INLINE _q #-}

-- | Order of BrainpoolP256T1 curve.
_r :: Integer
_r = 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP256T1 curve.
_x :: Fq
_x = 0xa3e8eb3cc1cfe7b7732213b23a656149afa142c47aafbc2b79a191562e1305f4
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP256T1 curve.
_y :: Fq
_y = 0x2d996c823439c56d7f7b22e14644417e69bcb6de39d027001dabe8f35b25c9be
{-# INLINE _y #-}

-- | Generator of affine BrainpoolP256T1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian BrainpoolP256T1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective BrainpoolP256T1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
