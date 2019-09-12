module Data.Curve.Weierstrass.BN254T
  ( module Data.Curve.Weierstrass
  -- * BN254T curve
  , module Data.Curve.Weierstrass.BN254T
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.BN254 (BN254, Fq)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field of points of BN254T curve.
type Fq2 = Extension U Fq
data U
instance IrreducibleMonic U Fq where
  poly _ = X2 + 1
  {-# INLINABLE poly #-}

-- | Field of coefficients of BN254T curve.
type Fr = Prime R
type R = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001

-- BN254T curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254 Fq2 Fr => WCurve c BN254 Fq2 Fr where
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

-- | Affine BN254T curve point.
type PA = WAPoint BN254 Fq2 Fr

-- Affine BN254T curve is a Weierstrass affine curve.
instance WACurve BN254 Fq2 Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN254T point.
type PJ = WJPoint BN254 Fq2 Fr

-- Jacobian BN254T curve is a Weierstrass Jacobian curve.
instance WJCurve BN254 Fq2 Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN254T point.
type PP = WPPoint BN254 Fq2 Fr

-- Projective BN254T curve is a Weierstrass projective curve.
instance WPCurve BN254 Fq2 Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254T curve.
_a :: Fq2
_a = toE' [
          ]
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN254T curve.
_b :: Fq2
_b = toE' [ 0x2b149d40ceb8aaae81be18991be06ac3b5b4c5e559dbefa33267e6dc24a138e5
          , 0x9713b03af0fed4cd2cafadeed8fdf4a74fa084e52d1852e4a2bd0685c315d2
          ]
{-# INLINABLE _b #-}

-- | Cofactor of BN254T curve.
_h :: Natural
_h = 0x30644e72e131a029b85045b68181585e06ceecda572a2489345f2299c0f9fa8d
{-# INLINABLE _h #-}

-- | Characteristic of BN254T curve.
_q :: Natural
_q = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINABLE _q #-}

-- | Order of BN254T curve.
_r :: Natural
_r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN254T curve.
_x :: Fq2
_x = toE' [ 0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
          , 0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2
          ]
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN254T curve.
_y :: Fq2
_y = toE' [ 0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
          , 0x90689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b
          ]
{-# INLINABLE _y #-}

-- | Generator of affine BN254T curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN254T curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN254T curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
