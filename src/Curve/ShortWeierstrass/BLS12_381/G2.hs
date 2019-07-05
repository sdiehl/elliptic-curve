module Curve.ShortWeierstrass.BLS12_381.G2
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fp2
  , P
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList, x)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)
import Curve.ShortWeierstrass.BLS12_381.G1 (Fp)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BLS12-381 curve G2
data G2

-- | Field of BLS12-381 curve G2
data PolynomialU
instance IrreducibleMonic Fp PolynomialU where
  split _ = x ^ (2 :: Int) + 1
type Fp2 = ExtensionField Fp PolynomialU

-- | BLS12-381 curve G2 is a short Weierstrass curve
instance SWCurve G2 Fp2 where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = fromList [4, 4]
  {-# INLINE _b #-}
  _g   = A
    ( fromList
      [ 0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
      , 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e
      ]
    )
    ( fromList
      [ 0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
      , 0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be
      ]
    )
  {-# INLINE _g #-}
  _h _ = 0x5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e5
  {-# INLINE _h #-}
  _n _ = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  {-# INLINE _n #-}
  _p _ = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
  {-# INLINE _p #-}

-- | Point of BLS12-381 curve G2
type P = SWPoint G2 Fp2
