module EllipticCurve.BLS12_381
  ( Fq
  , Fq2
  , Point_G1
  , Point_G2
  , P(..)
  , _b1
  , _b2
  , _h1
  , _h2
  , _q
  , _r
  ) where

import Protolude

import PrimeField (PrimeField)
import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList, x)

import EllipticCurve (EC(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Prime field of BLS12-381 curve @Fq@
type Fq = PrimeField 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

-- | BLS12-381 curve G1 @y^2=x^3+4@
data BLS12_381_G1
type Point_G1 = P BLS12_381_G1 Fq

-- | Extension field of BLS12-381 curve @Fq2=Fq[u]/<u^2+1>@
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = x ^ (2 :: Int) + 1
type Fq2 = ExtensionField Fq PolynomialU

-- | BLS12-381 curve G2 @y^2=x^3+4(1+i)@
data BLS12_381_G2
type Point_G2 = P BLS12_381_G2 Fq2

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | BLS12-381 curve G1 is an elliptic curve over Fq
instance EC BLS12_381_G1 Fq where
  data P BLS12_381_G1 Fq = A_G1 Fq Fq -- ^ Affine point
                         | O_G1       -- ^ Infinite point
    deriving (Eq, Show, Generic, NFData)

  id = O_G1
  {-# INLINE id #-}

  inv O_G1       = O_G1
  inv (A_G1 y z) = A_G1 y (-z)
  {-# INLINE inv #-}

  add p O_G1       = p
  add O_G1 q       = q
  add (A_G1 x1 y1) (A_G1 x2 y2)
    | x1 /= x2     = A_G1 x3a y3a
    | y1 + y2 /= 0 = A_G1 x3d y3d
    | otherwise    = O_G1
    where
      l_a = (y1 - y2) / n_a
      m_a = (x1 * y2 - x2 * y1) / n_a
      n_a = x1 - x2
      x3a = l_a ^ (2 :: Int) - x1 - x2
      y3a = -l_a * x3a - m_a
      l_d = (3 * x1 ^ (2 :: Int)) / n_d
      m_d = (-x1 ^ (3 :: Int) + 2 * _b1) / n_d
      n_d = 2 * y1
      x3d = l_d ^ (2 :: Int) - x1 - x2
      y3d = -l_d * x3d - m_d
  {-# INLINE add #-}

  def O_G1       = True
  def (A_G1 y z) = z * z == y * y * y + _b1
  {-# INLINE def #-}

-- | BLS12-381 curve G2 is an elliptic curve over Fq2
instance EC BLS12_381_G2 Fq2 where
  data P BLS12_381_G2 Fq2 = A_G2 Fq2 Fq2 -- ^ Affine point
                          | O_G2         -- ^ Infinite point
    deriving (Eq, Show, Generic, NFData)

  id = O_G2
  {-# INLINE id #-}

  inv O_G2       = O_G2
  inv (A_G2 y z) = A_G2 y (-z)
  {-# INLINE inv #-}

  add p O_G2       = p
  add O_G2 q       = q
  add (A_G2 x1 y1) (A_G2 x2 y2)
    | x1 /= x2     = A_G2 x3a y3a
    | y1 + y2 /= 0 = A_G2 x3d y3d
    | otherwise    = O_G2
    where
      l_a = (y1 - y2) / n_a
      m_a = (x1 * y2 - x2 * y1) / n_a
      n_a = x1 - x2
      x3a = l_a ^ (2 :: Int) - x1 - x2
      y3a = -l_a * x3a - m_a
      l_d = (3 * x1 ^ (2 :: Int)) / n_d
      m_d = (-x1 ^ (3 :: Int) + 2 * _b2) / n_d
      n_d = 2 * y1
      x3d = l_d ^ (2 :: Int) - x1 - x2
      y3d = -l_d * x3d - m_d
  {-# INLINE add #-}

  def O_G2       = True
  def (A_G2 y z) = z * z == y * y * y + _b2
  {-# INLINE def #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient of BLS12-381 curve G1
_b1 :: Fq
_b1 = 4
{-# INLINE _b1 #-}

-- | Coefficient of BLS12-381 curve G1
_b2 :: Fq2
_b2 = fromList [4, 1]
{-# INLINE _b2 #-}

-- | Cofactor of BLS12-381 curve G1
_h1 :: Integer
_h1 = 0x396c8c005555e1568c00aaab0000aaa
{-# INLINE _h1 #-}

-- | Cofactor of BLS12-381 curve G2
_h2 :: Integer
_h2 = 0x5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e
{-# INLINE _h2 #-}

-- | Characteristic of BLS12-381 curve
_q :: Integer
_q = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
{-# INLINE _q #-}

-- | Order of BLS12-381 curve
_r :: Integer
_r = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINE _r #-}
