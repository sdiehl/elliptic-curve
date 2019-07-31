module Curve.Weierstrass.BN254T
  ( Curve(..)
  , Fq2
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
  , gP
  ) where

import Protolude

import ExtensionField
import PrimeField

import Curve.Weierstrass
import Curve.Weierstrass.BN254 (Fq)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254T curve.
data BN254T

-- | Field of points of BN254T curve.
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = x * x + 1
type Fq2 = ExtensionField Fq PolynomialU

-- | Field of coefficients of BN254T curve.
type Fr = PrimeField 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001

-- | BN254T curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254T Fq2 => WCurve c BN254T Fq2 where
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

-- | Affine BN254T curve point.
type PA = WAPoint BN254T Fq2

-- | Affine BN254T curve is a Weierstrass affine curve.
instance WACurve BN254T Fq2 where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective BN254T point.
type PP = WPPoint BN254T Fq2

-- | Projective BN254T curve is a Weierstrass projective curve.
instance WPCurve BN254T Fq2 where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254T curve.
_a :: Fq2
_a = fromList [
              ]
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254T curve.
_b :: Fq2
_b = fromList [ 0x2b149d40ceb8aaae81be18991be06ac3b5b4c5e559dbefa33267e6dc24a138e5
              , 0x9713b03af0fed4cd2cafadeed8fdf4a74fa084e52d1852e4a2bd0685c315d2
              ]
{-# INLINE _b #-}

-- | Cofactor of BN254T curve.
_h :: Integer
_h = 0x30644e72e131a029b85045b68181585e06ceecda572a2489345f2299c0f9fa8d
{-# INLINE _h #-}

-- | Characteristic of BN254T curve.
_q :: Integer
_q = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _q #-}

-- | Order of BN254T curve.
_r :: Integer
_r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINE _r #-}

-- | Coordinate @X@ of BN254T curve.
_x :: Fq2
_x = fromList [ 0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
              , 0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2
              ]
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN254T curve.
_y :: Fq2
_y = fromList [ 0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
              , 0x90689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b
              ]
{-# INLINE _y #-}

-- | Affine generator of BN254T curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Projective generator of BN254T curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
