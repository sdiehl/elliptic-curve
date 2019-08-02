module Curve.Binary
  ( BCurve(..)
  , BPoint
  , BACurve(..)
  , BAPoint
  , BPCurve(..)
  , BPPoint
  , Coordinates(..)
  , Curve(..)
  , Form(..)
  , Group(..)
  , pattern A
  , pattern P
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..), Form(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Binary form
-------------------------------------------------------------------------------

-- | Binary points.
type BPoint = Point 'Binary

-- | Binary curves.
class (GaloisField k, Curve 'Binary c e k) => BCurve c e k where
  {-# MINIMAL a_, b_, h_, p_, r_, x_, y_ #-}
  a_ :: BPoint c e k -> k       -- ^ Coefficient @A@.
  b_ :: BPoint c e k -> k       -- ^ Coefficient @B@.
  h_ :: BPoint c e k -> Integer -- ^ Curve cofactor.
  p_ :: BPoint c e k -> Integer -- ^ Curve polynomial.
  r_ :: BPoint c e k -> Integer -- ^ Curve order.
  x_ :: BPoint c e k -> k       -- ^ Coordinate @X@.
  y_ :: BPoint c e k -> k       -- ^ Coordinate @Y@.

-- | Binary coordinates.
data Coordinates = Affine
                 | Projective

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Binary affine points.
type BAPoint = BPoint 'Affine

-- | Binary affine curves @y^2 + xy = x^3 + Ax^2 + B@.
class BCurve 'Affine e k => BACurve e k where
  {-# MINIMAL gA_ #-}
  gA_ :: BAPoint e k -- ^ Curve generator.

-- | Binary affine points patterns.
pattern A :: BACurve e k => k -> k -> BAPoint e k
pattern A x y <- A' x y

-- Binary affine curves are elliptic curves.
instance BACurve e k => Curve 'Binary 'Affine e k where

  data instance Point 'Binary 'Affine e k = A' k k -- ^ Affine point.
                                          | O      -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = const 2
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = b_ (witness :: BAPoint e k)
  {-# INLINE disc #-}

  point x y = let p = A' x y in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = A' x <$> yX (witness :: BAPoint e k) x
  {-# INLINE pointX #-}

  yX _ 0 = sr (b_ (witness :: BAPoint e k))
  yX _ x = quad 1 x ((x + a) * pow x 2 + b)
    where
      a = a_ (witness :: BAPoint e k)
      b = b_ (witness :: BAPoint e k)
  {-# INLINE yX #-}

-- Binary affine points are groups.
instance BACurve e k => Group (BAPoint e k) where

  add p O       = p
  add O q       = q
  add (A' x1 y1) (A' x2 y2)
    | xx == 0   = O
    | otherwise = A' x3 y3
    where
      xx = x1 + x2
      yy = y1 + y2
      l  = yy / xx
      x3 = pow l 2 + l + xx + a_ (witness :: BAPoint e k)
      y3 = l * (x1 + x3) + x3 + y1
  {-# INLINE add #-}

  dbl O         = O
  dbl (A' x y)
    | x == 0    = O
    | otherwise = A' x' y'
    where
      l  = x + y / x
      l' = l + 1
      x' = l * l' + a_ (witness :: BAPoint e k)
      y' = pow x 2 + l' * x'
  {-# INLINE dbl #-}

  def O        = True
  def (A' x y) = ((x + a) * x + y) * x + b + pow y 2 == 0
    where
      a = a_ (witness :: BAPoint e k)
      b = b_ (witness :: BAPoint e k)
  {-# INLINE def #-}

  gen = gA_
  {-# INLINE gen #-}

  id = O
  {-# INLINE id #-}

  inv O        = O
  inv (A' x y) = A' x (x + y)
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Binary affine points are arbitrary.
instance BACurve e k => Arbitrary (BAPoint e k) where
  arbitrary = return gA_ -- TODO
  -- arbitrary = mul gA' <$> (arbitrary :: Gen Int)
  -- arbitrary = suchThatMap arbitrary pointX

-- Binary affine points are pretty.
instance BACurve e k => Pretty (BAPoint e k) where
  pretty (A' x y) = pretty (x, y)
  pretty O        = "O"

-- Binary affine points are random.
instance BACurve e k => Random (BAPoint e k) where
  random g = case pointX x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  {-# INLINE random #-}
  randomR  = panic "not implemented."

-------------------------------------------------------------------------------
-- Projective coordinates
-------------------------------------------------------------------------------

-- | Binary projective points.
type BPPoint = BPoint 'Projective

-- | Binary projective curves @y^2z + xyz = x^3 + Ax^2z + Bz@.
class BCurve 'Projective e k => BPCurve e k where
  {-# MINIMAL gP_ #-}
  gP_ :: BPPoint e k -- ^ Curve generator.

-- | Binary projective points patterns.
pattern P :: BPCurve e k => k -> k -> k -> BPPoint e k
pattern P x y z <- P' x y z

-- Binary projective curves are elliptic curves.
instance BPCurve e k => Curve 'Binary 'Projective e k where

  data instance Point 'Binary 'Projective e k = P' k k k -- ^ Projective point.
    deriving (Generic, NFData, Read, Show)

  char = const 2
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = b_ (witness :: BPPoint e k)
  {-# INLINE disc #-}

  point x y = let p = P' x y 1 in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = flip (P' x) 1 <$> yX (witness :: BPPoint e k) x
  {-# INLINE pointX #-}

  yX _ 0 = sr (b_ (witness :: BPPoint e k))
  yX _ x = quad 1 x ((x + a) * pow x 2 + b)
    where
      a = a_ (witness :: BPPoint e k)
      b = b_ (witness :: BPPoint e k)
  {-# INLINE yX #-}

-- Binary projective points are groups.
instance BPCurve e k => Group (BPPoint e k) where

  -- | Addition formula add-2008-bl
  add  p            (P'  _  _  0) = p
  add (P'  _  _  0)  q            = q
  add (P' x1 y1 z1) (P' x2 y2 z2) = P' x3 y3 z3
    where
      y1z2 = y1 * z2
      x1z2 = x1 * z2
      a    = y1z2 + z1 * y2
      b    = x1z2 + z1 * x2
      ab   = a + b
      c    = pow b 2
      d    = z1 * z2
      e    = b * c
      f    = (a * ab + a_ (witness :: BPPoint e k) * c) * d + e
      x3   = b * f
      y3   = c * (a * x1z2 + b * y1z2) + ab * f
      z3   = e * d
  {-# INLINE add #-}

  -- | Doubling formula dbl-2008-bl
  dbl (P'  _  _  0) = P'  0  1  0
  dbl (P' x1 y1 z1) = P' x3 y3 z3
    where
      a  = pow x1 2
      b  = a + y1 * z1
      c  = x1 * z1
      bc = b + c
      d  = pow c 2
      e  = b * bc + a_ (witness :: BPPoint e k) * d
      x3 = c * e
      y3 = bc * e + pow a 2 * c
      z3 = c * d
  {-# INLINE dbl #-}

  def (P' x y z) = ((x + a * z) * x + yz) * x + b * pow z 3 + y * yz == 0
    where
      a  = a_ (witness :: BPPoint e k)
      b  = b_ (witness :: BPPoint e k)
      yz = y * z
  {-# INLINE def #-}

  gen = gP_
  {-# INLINE gen #-}

  id = P' 0 1 0
  {-# INLINE id #-}

  inv (P' x y z) = P' x (x + y) z
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Binary projective points are arbitrary.
instance BPCurve e k => Arbitrary (BPPoint e k) where
  arbitrary = return gP_ -- TODO
  -- arbitrary = mul gP_ <$> (arbitrary :: Gen Int)
  -- arbitrary = suchThatMap arbitrary pointX

-- Binary projective points are equatable.
instance BPCurve e k => Eq (BPPoint e k) where
  P' x1 y1 z1 == P' x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * z2 == x2 * z1 && y1 * z2 == y2 * z1

-- Binary projective points are pretty.
instance BPCurve e k => Pretty (BPPoint e k) where
  pretty (P' x y z) = pretty (x, y, z)

-- Binary projective points are random.
instance BPCurve e k => Random (BPPoint e k) where
  random g = case pointX x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  {-# INLINE random #-}
  randomR  = panic "not implemented."
