module Curve.ShortWeierstrass.SECP112R2
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fp
  , P
  ) where

import PrimeField (PrimeField)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP112R2 curve
data SECP112R2

-- | Field of SECP112R2 curve
type Fp = PrimeField 0xdb7c2abf62e35e668076bead208b

-- | SECP112R2 curve is a short Weierstrass curve
instance SWCurve SECP112R2 Fp where
  _a _ = 0x6127c24c05f38a0aaaf65c0ef02c
  {-# INLINE _a #-}
  _b _ = 0x51def1815db5ed74fcc34c85d709
  {-# INLINE _b #-}
  _g   = A 0x4ba30ab5e892b4e1649dd0928643
           0xadcd46f5882e3747def36e956e97
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _n _ = 0x36df0aafd8b8d7597ca10520d04b
  {-# INLINE _n #-}
  _p _ = 0xdb7c2abf62e35e668076bead208b
  {-# INLINE _p #-}

-- | Point of SECP112R2 curve
type P = SWPoint SECP112R2 Fp
