{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Field (F(..), setF) where

import Protolude

-- | Field
newtype F = F { getF :: Integer } deriving (Show, Generic, NFData)

-- | Characteristic of field
_q :: Integer
_q = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | Field is a ring
instance Num F where
  F a + F b    = setF $ a + b
  F a * F b    = setF $ a * b
  abs (F a)    = setF a -- ^ ignore
  signum (F a) = setF a -- ^ ignore
  fromInteger  = setF
  negate (F a) = setF (-a)

-- | Field constructor
setF :: Integer -> F
setF = F . flip mod _q

-- | Field is a division ring
instance Fractional F where
  fromRational (a :% b) = setF a / setF b
  recip                 = inv

-- | Inverse in field
{-# INLINE inv #-}
inv :: F -> F
inv (F a) = case extGCD _q a of
  (1, (b, _)) -> setF b
  _           -> panic "Constant _q is not prime."
  where
    extGCD :: Integral a => a -> a -> (a, (a, a))
    extGCD y 0 = (y, (0, 1))
    extGCD y x = (g, (t - s * q, s))
      where
        (q, r)      = quotRem y x
        (g, (s, t)) = extGCD x r
