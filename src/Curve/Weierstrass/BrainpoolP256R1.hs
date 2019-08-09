module Curve.Weierstrass.BrainpoolP256R1
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

-- | BrainpoolP256R1 curve.
data BrainpoolP256R1

-- | Field of points of BrainpoolP256R1 curve.
type Fq = PrimeField 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377

-- | Field of coefficients of BrainpoolP256R1 curve.
type Fr = PrimeField 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7

-- | BrainpoolP256R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP256R1 Fq Fr => WCurve c BrainpoolP256R1 Fq Fr where
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

-- | Affine BrainpoolP256R1 curve point.
type PA = WAPoint BrainpoolP256R1 Fq Fr

-- | Affine BrainpoolP256R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP256R1 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BrainpoolP256R1 point.
type PJ = WJPoint BrainpoolP256R1 Fq Fr

-- | Jacobian BrainpoolP256R1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP256R1 Fq Fr where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BrainpoolP256R1 point.
type PP = WPPoint BrainpoolP256R1 Fq Fr

-- | Projective BrainpoolP256R1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP256R1 Fq Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP256R1 curve.
_a :: Fq
_a = 0x7d5a0975fc2c3057eef67530417affe7fb8055c126dc5c6ce94a4b44f330b5d9
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP256R1 curve.
_b :: Fq
_b = 0x26dc5c6ce94a4b44f330b5d9bbd77cbf958416295cf7e1ce6bccdc18ff8c07b6
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP256R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP256R1 curve.
_q :: Integer
_q = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377
{-# INLINE _q #-}

-- | Order of BrainpoolP256R1 curve.
_r :: Integer
_r = 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP256R1 curve.
_x :: Fq
_x = 0x8bd2aeb9cb7e57cb2c4b482ffc81b7afb9de27e1e3bd23c23a4453bd9ace3262
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP256R1 curve.
_y :: Fq
_y = 0x547ef835c3dac4fd97f8461a14611dc9c27745132ded8e545c1d54c72f046997
{-# INLINE _y #-}

-- | Generator of affine BrainpoolP256R1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian BrainpoolP256R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective BrainpoolP256R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
