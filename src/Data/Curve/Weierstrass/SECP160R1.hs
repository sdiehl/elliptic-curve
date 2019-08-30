module Data.Curve.Weierstrass.SECP160R1
  ( module Data.Curve.Weierstrass
  -- * SECP160R1 curve
  , module Data.Curve.Weierstrass.SECP160R1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.Base (WCurve(..), WACurve(..), WJCurve(..), WPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP160R1 curve.
data SECP160R1

-- | Field of points of SECP160R1 curve.
type Fq = Prime 0xffffffffffffffffffffffffffffffff7fffffff

-- | Field of coefficients of SECP160R1 curve.
type Fr = Prime 0x100000000000000000001f4c8f927aed3ca752257

-- SECP160R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP160R1 Fq Fr => WCurve c SECP160R1 Fq Fr where
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

-- | Affine SECP160R1 curve point.
type PA = WAPoint SECP160R1 Fq Fr

-- Affine SECP160R1 curve is a Weierstrass affine curve.
instance WACurve SECP160R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP160R1 point.
type PJ = WJPoint SECP160R1 Fq Fr

-- Jacobian SECP160R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP160R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP160R1 point.
type PP = WPPoint SECP160R1 Fq Fr

-- Projective SECP160R1 curve is a Weierstrass projective curve.
instance WPCurve SECP160R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP160R1 curve.
_a :: Fq
_a = 0xffffffffffffffffffffffffffffffff7ffffffc
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP160R1 curve.
_b :: Fq
_b = 0x1c97befc54bd7a8b65acf89f81d4d4adc565fa45
{-# INLINABLE _b #-}

-- | Cofactor of SECP160R1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of SECP160R1 curve.
_q :: Natural
_q = 0xffffffffffffffffffffffffffffffff7fffffff
{-# INLINABLE _q #-}

-- | Order of SECP160R1 curve.
_r :: Natural
_r = 0x100000000000000000001f4c8f927aed3ca752257
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP160R1 curve.
_x :: Fq
_x = 0x4a96b5688ef573284664698968c38bb913cbfc82
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP160R1 curve.
_y :: Fq
_y = 0x23a628553168947d59dcc912042351377ac5fb32
{-# INLINABLE _y #-}

-- | Generator of affine SECP160R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP160R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP160R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
