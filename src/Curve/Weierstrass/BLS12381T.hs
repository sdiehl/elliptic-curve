module Curve.Weierstrass.BLS12381T
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.BLS12381T
  ) where

import Protolude

import ExtensionField
import PrimeField

import Curve.Weierstrass
import Curve.Weierstrass.BLS12381 (Fq)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BLS12381T curve.
data BLS12381T

-- | Field of points of BLS12381T curve.
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = X2 + 1
type Fq2 = ExtensionField Fq PolynomialU

-- | Field of coefficients of BLS12381T curve.
type Fr = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | BLS12381T curve is a Weierstrass curve.
instance Curve 'Weierstrass c BLS12381T Fq2 Fr => WCurve c BLS12381T Fq2 Fr where
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

-- | Affine BLS12381T curve point.
type PA = WAPoint BLS12381T Fq2 Fr

-- | Affine BLS12381T curve is a Weierstrass affine curve.
instance WACurve BLS12381T Fq2 Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BLS12381T point.
type PJ = WJPoint BLS12381T Fq2 Fr

-- | Jacobian BLS12381T curve is a Weierstrass Jacobian curve.
instance WJCurve BLS12381T Fq2 Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BLS12381T point.
type PP = WPPoint BLS12381T Fq2 Fr

-- | Projective BLS12381T curve is a Weierstrass projective curve.
instance WPCurve BLS12381T Fq2 Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BLS12381T curve.
_a :: Fq2
_a = toField [
             ]
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BLS12381T curve.
_b :: Fq2
_b = toField [ 0x4
             , 0x4
             ]
{-# INLINABLE _b #-}

-- | Cofactor of BLS12381T curve.
_h :: Integer
_h = 0x5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e5
{-# INLINABLE _h #-}

-- | Characteristic of BLS12381T curve.
_q :: Integer
_q = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
{-# INLINABLE _q #-}

-- | Order of BLS12381T curve.
_r :: Integer
_r = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BLS12381T curve.
_x :: Fq2
_x = toField [ 0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
             , 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e
             ]
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BLS12381T curve.
_y :: Fq2
_y = toField [ 0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
             , 0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be
             ]
{-# INLINABLE _y #-}

-- | Generator of affine BLS12381T curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BLS12381T curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BLS12381T curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
