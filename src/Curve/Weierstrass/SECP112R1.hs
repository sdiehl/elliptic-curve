module Curve.Weierstrass.SECP112R1
  ( Fp
  , P
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP112R1 curve
data SECP112R1

-- | Field of SECP112R1 curve
type Fp = PrimeField 0xdb7c2abf62e35e668076bead208b

-- | SECP112R1 curve is a Weierstrass curve
instance WCurve SECP112R1 Fp where
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

-- | Point of SECP112R1 curve
type P = WPoint SECP112R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP112R1 curve
_a :: Fp
_a = 0xdb7c2abf62e35e668076bead2088
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP112R1 curve
_b :: Fp
_b = 0x659ef8ba043916eede8911702b22
{-# INLINE _b #-}

-- | Generator of SECP112R1 curve
_g :: P
_g = A
     0x9487239995a5ee76b55f9c2f098
     0xa89ce5af8724c0a23e0e0ff77500
{-# INLINE _g #-}

-- | Cofactor of SECP112R1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of SECP112R1 curve
_n :: Integer
_n = 0xdb7c2abf62e35e7628dfac6561c5
{-# INLINE _n #-}

-- | Characteristic of SECP112R1 curve
_p :: Integer
_p = 0xdb7c2abf62e35e668076bead208b
{-# INLINE _p #-}
