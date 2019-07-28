module Curve.Weierstrass.BN254AT
  ( Curve(..)
  , Fq2
  , Fr
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
  , _a
  , _b
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import ExtensionField
import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Curve.Weierstrass.BN254A (Fq)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254AT curve.
data BN254AT

-- | Field of points of BN254AT curve.
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = x * x + 5
type Fq2 = ExtensionField Fq PolynomialU

-- | Field of coefficients of BN254AT curve.
type Fr = PrimeField 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001

-- | BN254AT curve is a Weierstrass curve.
instance WCurve BN254AT Fq2 where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
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

-- | Point of BN254AT curve.
type P = WPoint BN254AT Fq2

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254AT curve.
_a :: Fq2
_a = fromList [
              ]
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254AT curve.
_b :: Fq2
_b = fromList [ 0x0
              , 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000000
              ]
{-# INLINE _b #-}

-- | Generator of BN254AT curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BN254AT curve.
_h :: Integer
_h = 0x2370fb049d410fbe4e761a9886e50241dc42cf101e0000017e80600000000001
{-# INLINE _h #-}

-- | Characteristic of BN254AT curve.
_q :: Integer
_q = 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001
{-# INLINE _q #-}

-- | Order of BN254AT curve.
_r :: Integer
_r = 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
{-# INLINE _r #-}

-- | Coordinate @X@ of BN254AT curve.
_x :: Fq2
_x = fromList [ 0x19b0bea4afe4c330da93cc3533da38a9f430b471c6f8a536e81962ed967909b5
              , 0xa1cf585585a61c6e9880b1f2a5c539f7d906fff238fa6341e1de1a2e45c3f72
              ]
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN254AT curve.
_y :: Fq2
_y = fromList [ 0x17abd366ebbd65333e49c711a80a0cf6d24adf1b9b3990eedcc91731384d2627
              , 0xee97d6de9902a27d00e952232a78700863bc9aa9be960c32f5bf9fd0a32d345
              ]
{-# INLINE _y #-}
