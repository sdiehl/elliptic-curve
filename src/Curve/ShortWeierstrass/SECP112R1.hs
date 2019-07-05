module Curve.ShortWeierstrass.SECP112R1
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fq
  , P
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP112R1 curve
data SECP112R1

-- | Field of SECP112R1 curve
type Fq = PrimeField 0xDB7C2ABF62E35E668076BEAD208B

-- | SECP112R1 curve is a short Weierstrass curve
instance SWCurve SECP112R1 Fq where
  _a _ = 0xDB7C2ABF62E35E668076BEAD2088
  {-# INLINE _a #-}
  _b _ = 0x659EF8BA043916EEDE8911702B22
  {-# INLINE _b #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _q _ = 0xDB7C2ABF62E35E668076BEAD208B
  {-# INLINE _q #-}
  _r _ = 0xDB7C2ABF62E35E7628DFAC6561C5
  {-# INLINE _r #-}

-- | Point of SECP112R1 curve
type P = SWPoint SECP112R1 Fq
