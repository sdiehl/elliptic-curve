module Data.Curve.Weierstrass.BN384
  ( module Data.Curve.Weierstrass
  , module Data.Curve.Weierstrass.BN384
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN384 curve.
data BN384

-- | Field of points of BN384 curve.
type Fq = Prime 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b

-- | Field of coefficients of BN384 curve.
type Fr = Prime 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca023c8fbe29531892c795356487d8ac63e4f4db17384341a5775

-- | BN384 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN384 Fq Fr => WCurve c BN384 Fq Fr where
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

-- | Affine BN384 curve point.
type PA = WAPoint BN384 Fq Fr

-- | Affine BN384 curve is a Weierstrass affine curve.
instance WACurve BN384 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN384 point.
type PJ = WJPoint BN384 Fq Fr

-- | Jacobian BN384 curve is a Weierstrass Jacobian curve.
instance WJCurve BN384 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN384 point.
type PP = WPPoint BN384 Fq Fr

-- | Projective BN384 curve is a Weierstrass projective curve.
instance WPCurve BN384 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN384 curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN384 curve.
_b :: Fq
_b = 0x3
{-# INLINABLE _b #-}

-- | Cofactor of BN384 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BN384 curve.
_q :: Natural
_q = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b
{-# INLINABLE _q #-}

-- | Order of BN384 curve.
_r :: Natural
_r = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca023c8fbe29531892c795356487d8ac63e4f4db17384341a5775
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN384 curve.
_x :: Fq
_x = 0x1
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN384 curve.
_y :: Fq
_y = 0x2
{-# INLINABLE _y #-}

-- | Generator of affine BN384 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN384 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN384 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
