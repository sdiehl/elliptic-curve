module Data.Curve.Weierstrass.BLS48581
  ( module Data.Curve.Weierstrass
  , module Data.Curve.Weierstrass.BLS48581
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BLS48581 curve.
data BLS48581

-- | Field of points of BLS48581 curve.
type Fq = Prime 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b

-- | Field of coefficients of BLS48581 curve.
type Fr = Prime 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01

-- | BLS48581 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BLS48581 Fq Fr => WCurve c BLS48581 Fq Fr where
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

-- | Affine BLS48581 curve point.
type PA = WAPoint BLS48581 Fq Fr

-- | Affine BLS48581 curve is a Weierstrass affine curve.
instance WACurve BLS48581 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BLS48581 point.
type PJ = WJPoint BLS48581 Fq Fr

-- | Jacobian BLS48581 curve is a Weierstrass Jacobian curve.
instance WJCurve BLS48581 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BLS48581 point.
type PP = WPPoint BLS48581 Fq Fr

-- | Projective BLS48581 curve is a Weierstrass projective curve.
instance WPCurve BLS48581 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BLS48581 curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BLS48581 curve.
_b :: Fq
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of BLS48581 curve.
_h :: Natural
_h = 0x85555841aaaec4ac
{-# INLINABLE _h #-}

-- | Characteristic of BLS48581 curve.
_q :: Natural
_q = 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b
{-# INLINABLE _q #-}

-- | Order of BLS48581 curve.
_r :: Natural
_r = 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BLS48581 curve.
_x :: Fq
_x = 0x2af59b7ac340f2baf2b73df1e93f860de3f257e0e86868cf61abdbaedffb9f7544550546a9df6f9645847665d859236ebdbc57db368b11786cb74da5d3a1e6d8c3bce8732315af640
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BLS48581 curve.
_y :: Fq
_y = 0xcefda44f6531f91f86b3a2d1fb398a488a553c9efeb8a52e991279dd41b720ef7bb7beffb98aee53e80f678584c3ef22f487f77c2876d1b2e35f37aef7b926b576dbb5de3e2587a70
{-# INLINABLE _y #-}

-- | Generator of affine BLS48581 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BLS48581 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BLS48581 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
