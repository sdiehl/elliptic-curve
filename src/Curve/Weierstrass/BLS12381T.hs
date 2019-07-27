module Curve.Weierstrass.BLS12381T
  ( Curve(..)
  , Fp2
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import ExtensionField

import Curve (Curve(..), Group(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Curve.Weierstrass.BLS12381 (Fp)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BLS12381T curve.
data BLS12381T

-- | Field of BLS12381T curve.
data PolynomialU
instance IrreducibleMonic Fp PolynomialU where
  split _ = x * x + 1
type Fp2 = ExtensionField Fp PolynomialU

-- | BLS12381T curve is a Weierstrass curve.
instance WCurve BLS12381T Fp2 where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of BLS12381T curve.
type P = WPoint BLS12381T Fp2

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BLS12381T curve.
_a :: Fp2
_a = fromList [
              ]
{-# INLINE _a #-}

-- | Coefficient @B@ of BLS12381T curve.
_b :: Fp2
_b = fromList [ 0x4
              , 0x4
              ]
{-# INLINE _b #-}

-- | Generator of BLS12381T curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BLS12381T curve.
_h :: Integer
_h = 0x5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e5
{-# INLINE _h #-}

-- | Order of BLS12381T curve.
_n :: Integer
_n = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINE _n #-}

-- | Characteristic of BLS12381T curve.
_p :: Integer
_p = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
{-# INLINE _p #-}

-- | Coordinate @X@ of BLS12381T curve.
_x :: Fp2
_x = fromList [ 0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
              , 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e
              ]
{-# INLINE _x #-}

-- | Coordinate @Y@ of BLS12381T curve.
_y :: Fp2
_y = fromList [ 0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
              , 0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be
              ]
{-# INLINE _y #-}
