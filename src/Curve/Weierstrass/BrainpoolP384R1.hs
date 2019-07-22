module Curve.Weierstrass.BrainpoolP384R1
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

-- | Brainpool-P384R1 curve
data BrainpoolP384R1

-- | Field of Brainpool-P384R1 curve
type Fp = PrimeField 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53

-- | Brainpool-P384R1 curve is a Weierstrass curve
instance WCurve BrainpoolP384R1 Fp where
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

-- | Point of Brainpool-P384R1 curve
type P = WPoint BrainpoolP384R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Brainpool-P384R1 curve
_a :: Fp
_a = 0x7bc382c63d8c150c3c72080ace05afa0c2bea28e4fb22787139165efba91f90f8aa5814a503ad4eb04a8c7dd22ce2826
{-# INLINE _a #-}

-- | Coefficient @B@ of Brainpool-P384R1 curve
_b :: Fp
_b = 0x4a8c7dd22ce28268b39b55416f0447c2fb77de107dcd2a62e880ea53eeb62d57cb4390295dbc9943ab78696fa504c11
{-# INLINE _b #-}

-- | Generator of Brainpool-P384R1 curve
_g :: P
_g = A
     0x1d1c64f068cf45ffa2a63a81b7c13f6b8847a3e77ef14fe3db7fcafe0cbd10e8e826e03436d646aaef87b2e247d4af1e
     0x8abe1d7520f9c2a45cb1eb8e95cfd55262b70b29feec5864e19c054ff99129280e4646217791811142820341263c5315
{-# INLINE _g #-}

-- | Cofactor of Brainpool-P384R1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of Brainpool-P384R1 curve
_n :: Integer
_n = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565
{-# INLINE _n #-}

-- | Characteristic of Brainpool-P384R1 curve
_p :: Integer
_p = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53
{-# INLINE _p #-}
