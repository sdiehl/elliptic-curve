module Curve.Weierstrass
  ( Coordinates(..)
  , Curve(..)
  , Form(..)
  , Group(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WPPoint
  , WPCurve(..)
  , dehom
  ) where

import Protolude

import Control.Monad.Random (Random(..), RandomGen)
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), Gen)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..), Form(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Weierstrass form
-------------------------------------------------------------------------------

-- | Weierstrass points.
type WPoint = Point 'Weierstrass

-- | Weierstrass curves.
class Curve 'Weierstrass c e k => WCurve c e k where
  {-# MINIMAL a_, b_, h_, q_, r_, x_, y_ #-}
  a_ :: WPoint c e k -> k       -- ^ Coefficient @A@.
  b_ :: WPoint c e k -> k       -- ^ Coefficient @B@.
  h_ :: WPoint c e k -> Integer -- ^ Curve cofactor.
  q_ :: WPoint c e k -> Integer -- ^ Curve characteristic.
  r_ :: WPoint c e k -> Integer -- ^ Curve order.
  x_ :: WPoint c e k -> k       -- ^ Coordinate @X@.
  y_ :: WPoint c e k -> k       -- ^ Coordinate @Y@.

-- | Weierstrass coordinates.
data Coordinates = Affine
                 | Projective

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Weierstrass affine points.
type WAPoint = WPoint 'Affine

-- | Weierstrass affine curves @y^2 = x^3 + Ax + B@.
class WCurve 'Affine e k => WACurve e k where
  {-# MINIMAL gA_ #-}
  gA_ :: WAPoint e k -- ^ Curve generator.

-- Weierstrass affine curves are elliptic curves.
instance (GaloisField k, WACurve e k) => Curve 'Weierstrass 'Affine e k where

  data instance Point 'Weierstrass 'Affine e k = A k k -- ^ Affine point.
                                               | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = 4 * pow a 3 + 27 * pow b 2
    where
      a = a_ (witness :: WAPoint e k)
      b = b_ (witness :: WAPoint e k)
  {-# INLINE disc #-}

  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = A x <$> yX (witness :: WAPoint e k) x
  {-# INLINE pointX #-}

  yX _ x = sr (((pow x 2 + a) * x) + b)
    where
      a = a_ (witness :: WAPoint e k)
      b = b_ (witness :: WAPoint e k)
  {-# INLINE yX #-}

-- Weierstrass affine points are groups.
instance (GaloisField k, WACurve e k) => Group (WAPoint e k) where

  add p O       = p
  add O q       = q
  add (A x1 y1) (A x2 y2)
    | x1 == x2  = O
    | otherwise = A x3 y3
    where
      l  = (y1 - y2) / (x1 - x2)
      x3 = pow l 2 - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINE add #-}

  dbl O         = O
  dbl (A x y)
    | y == 0    = O
    | otherwise = A x' y'
    where
      l  = (3 * pow x 2 + a_ (witness :: WAPoint e k)) / (2 * y)
      x' = pow l 2 - 2 * x
      y' = l * (x - x') - y
  {-# INLINE dbl #-}

  def O       = True
  def (A x y) = pow y 2 == (pow x 2 + a) * x + b
    where
      a = a_ (witness :: WAPoint e k)
      b = b_ (witness :: WAPoint e k)
  {-# INLINE def #-}

  gen = gA_
  {-# INLINE gen #-}

  id = O
  {-# INLINE id #-}

  inv O       = O
  inv (A x y) = A x (-y)
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Weierstrass affine points are arbitrary.
instance (GaloisField k, WACurve e k) => Arbitrary (WAPoint e k) where
  arbitrary = mul' gA_ <$> (arbitrary :: Gen Integer) -- TODO
  -- arbitrary = suchThatMap arbitrary pointX

-- Weierstrass affine points are pretty.
instance (GaloisField k, WACurve e k) => Pretty (WAPoint e k) where
  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-- Weierstrass affine points are random.
instance (GaloisField k, WACurve e k) => Random (WAPoint e k) where
  random  = first (mul' gA_) . (random :: RandomGen g => g -> (Integer, g)) -- TODO
  -- random g = case pointX x of
  --   Just p -> (p, g')
  --   _      -> random g'
  --   where
  --     (x, g') = random g
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Projective coordinates
-------------------------------------------------------------------------------

-- | Weierstrass projective points.
type WPPoint = WPoint 'Projective

-- | Weierstrass projective curves @y^2 = x^3 + Ax + B@.
class WCurve 'Projective e k => WPCurve e k where
  {-# MINIMAL gP_ #-}
  gP_ :: WPPoint e k -- ^ Curve generator.

-- Weierstrass projective curves are elliptic curves.
instance (GaloisField k, WPCurve e k) => Curve 'Weierstrass 'Projective e k where

  data instance Point 'Weierstrass 'Projective e k = P k k k -- ^ Projective point.
    deriving (Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = 4 * pow a 3 + 27 * pow b 2
    where
      a = a_ (witness :: WPPoint e k)
      b = b_ (witness :: WPPoint e k)
  {-# INLINE disc #-}

  point x y = let p = P x y 1 in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = flip (P x) 1 <$> yX (witness :: WPPoint e k) x
  {-# INLINE pointX #-}

  yX _ x = sr (((pow x 2 + a) * x) + b)
    where
      a = a_ (witness :: WPPoint e k)
      b = b_ (witness :: WPPoint e k)
  {-# INLINE yX #-}

-- Weierstrass projective points are groups.
instance (GaloisField k, WPCurve e k) => Group (WPPoint e k) where

  -- | Addition formula add-1998-cmo-2
  add  p           (P  _  _  0) = p
  add (P  _  _  0)  q           = q
  add (P x1 y1 z1) (P x2 y2 z2) = P x3 y3 z3
    where
      y1z2 = y1 * z2
      x1z2 = x1 * z2
      z1z2 = z1 * z2
      u    = y2 * z1 - y1z2
      uu   = pow u 2
      v    = x2 * z1 - x1z2
      vv   = pow v 2
      vvv  = v * vv
      r    = vv * x1z2
      a    = uu * z1z2 - vvv - 2 * r
      x3   = v * a
      y3   = u * (r - a) - vvv * y1z2
      z3   = vvv * z1z2
  {-# INLINE add #-}

  -- | Doubling formula dbl-2007-bl
  dbl (P  _  _  0) = P  0  1  0
  dbl (P x1 y1 z1) = P x3 y3 z3
    where
      xx  = pow x1 2
      zz  = pow z1 2
      w   = a_ (witness :: WPPoint e k) * zz + 3 * xx
      s   = 2 * y1 * z1
      ss  = pow s 2
      sss = s * ss
      r   = y1 * s
      rr  = pow r 2
      b   = pow (x1 + r) 2 - xx - rr
      h   = pow w 2 - 2 * b
      x3  = h * s
      y3  = w * (b - h) - 2 * rr
      z3  = sss
  {-# INLINE dbl #-}

  def (P x y z) = (pow x 2 + a * zz) * x == (pow y 2 - b * zz) * z
    where
      a  = a_ (witness :: WPPoint e k)
      b  = b_ (witness :: WPPoint e k)
      zz = pow z 2
  {-# INLINE def #-}

  gen = gP_
  {-# INLINE gen #-}

  id = P 0 1 0
  {-# INLINE id #-}

  inv (P x y z) = P x (-y) z
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Weierstrass projective points are arbitrary.
instance (GaloisField k, WPCurve e k) => Arbitrary (WPPoint e k) where
  arbitrary = mul' gP_ <$> (arbitrary :: Gen Integer) -- TODO
  -- arbitrary = suchThatMap arbitrary pointX

-- Weierstrass projective points are equatable.
instance (GaloisField k, WPCurve e k) => Eq (WPPoint e k) where
  p == p' = case (dehom p, dehom p') of
    (P x y z, P x' y' z') -> x == x' && y == y' && z == z'

-- Weierstrass projective points are pretty.
instance (GaloisField k, WPCurve e k) => Pretty (WPPoint e k) where
  pretty (P x y z) = pretty (x, y, z)

-- Weierstrass projective points are random.
instance (GaloisField k, WPCurve e k) => Random (WPPoint e k) where
  random  = first (mul' gP_) . (random :: RandomGen g => g -> (Integer, g)) -- TODO
  -- random g = case pointX x of
  --   Just p -> (p, g')
  --   _      -> random g'
  --   where
  --     (x, g') = random g
  {-# INLINE random #-}
  randomR = panic "not implemented."

-- | Dehomogenisation of Weierstrass projective points.
dehom :: (GaloisField k, WPCurve e k) => WPPoint e k -> WPPoint e k
dehom (P 0 _ 0) = P    0       1    0
dehom (P x y 0) = P    1    (y / x) 0
dehom (P x y z) = P (x / z) (y / z) 1
