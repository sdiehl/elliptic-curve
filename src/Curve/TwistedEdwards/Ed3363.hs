module Curve.TwistedEdwards.Ed3363
  -- | Types
  ( Fp
  , P
  -- | Parameters
  , _a
  , _d
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.TwistedEdwards (Point(..), TECurve(..), TEPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Ed3363 curve
data Ed3363

-- | Field of Ed3363 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd

-- | Ed3363 curve is a twisted Edwards curve
instance TECurve Ed3363 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Ed3363 curve
type P = TEPoint Ed3363 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Ed3363 curve
_a :: Fp
_a = 1
{-# INLINE _a #-}

-- | Coefficient @D@ of Ed3363 curve
_d :: Fp
_d = 0x2b67
{-# INLINE _d #-}

-- | Generator of Ed3363 curve
_g :: P
_g = A 0xc
       0xc0dc616b56502e18e1c161d007853d1b14b46c3811c7ef435b6db5d5650ca0365db12bec68505fe8632
{-# INLINE _g #-}

-- | Cofactor of Ed3363 curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Order of Ed3363 curve
_n :: Integer
_n = 0x100000000000000000000000000000000000000000038a0afd4c28605eb5c3fc9dd53d97cacb9f4fd4028
{-# INLINE _n #-}

-- | Characteristic of Ed3363 curve
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd
{-# INLINE _p #-}
