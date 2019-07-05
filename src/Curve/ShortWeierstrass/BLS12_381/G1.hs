module Curve.ShortWeierstrass.BLS12_381.G1
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

-- | BLS12-381 curve G1
data G1

-- | Prime field @Fq@ of BLS12-381 curve G1
type Fq = PrimeField 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

-- | BLS12-381 curve G1 is a short Weierstrass curve
instance SWCurve G1 Fq where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 4
  {-# INLINE _b #-}
  _g   = A
    3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507
    1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569
  {-# INLINE _g #-}
  _h _ = 0x396c8c005555e1568c00aaab0000aaa
  {-# INLINE _h #-}
  _q _ = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
  {-# INLINE _q #-}
  _r _ = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  {-# INLINE _r #-}

-- | Point of BLS12-381 curve G1
type P = SWPoint G1 Fq
