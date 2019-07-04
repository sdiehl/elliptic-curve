module Curve.ShortWeierstrass.BLS12_381.G2
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fq2
  , P
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList, x)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)
import Curve.ShortWeierstrass.BLS12_381.G1 (Fq)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BLS12-381 curve G2
data G2

-- | Extension field @Fq2 = Fq[u] / <u^2 + 1>@ of BLS12-381 curve G2
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = x ^ (2 :: Int) + 1
type Fq2 = ExtensionField Fq PolynomialU

-- | BLS12-381 curve G2 is a short Weierstrass curve
instance SWCurve G2 Fq2 where
  _a _   = 0
  {-# INLINE _a #-}
  _b _   = fromList [4, 4]
  {-# INLINE _b #-}
  _g     = A
    ( fromList
      [ 352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160
      , 3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758
      ]
    )
    ( fromList
      [ 1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905
      , 927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582
      ]
    )
  {-# INLINE _g #-}
  _h _ _ = 0x5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e5
  {-# INLINE _h #-}
  _q _ _ = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
  {-# INLINE _q #-}
  _r _ _ = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  {-# INLINE _r #-}

-- | Point of BLS12-381 curve G2
type P = SWPoint G2 Fq2
