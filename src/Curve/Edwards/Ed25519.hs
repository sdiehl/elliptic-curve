module Curve.Edwards.Ed25519
  ( Fp
  , P
  , _a
  , _d
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Edwards (ECurve(..), EPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Ed25519 curve
data Ed25519

-- | Field of Ed25519 curve
type Fp = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed

-- | Ed25519 curve is an Edwards curve
instance ECurve Ed25519 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Ed25519 curve
type P = EPoint Ed25519 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Ed25519 curve
_a :: Fp
_a = -1
{-# INLINE _a #-}

-- | Coefficient @D@ of Ed25519 curve
_d :: Fp
_d = 0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3
{-# INLINE _d #-}

-- | Generator of Ed25519 curve
_g :: P
_g = A
     0x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a
     0x6666666666666666666666666666666666666666666666666666666666666658
{-# INLINE _g #-}

-- | Cofactor of Ed25519 curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Order of Ed25519 curve
_n :: Integer
_n = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed
{-# INLINE _n #-}

-- | Characteristic of Ed25519 curve
_p :: Integer
_p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
{-# INLINE _p #-}
