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
  , Point(..)
  , fromAtoP
  , fromPtoA
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import PrimeField (PrimeField)
import Test.Tasty.QuickCheck (Arbitrary(..), suchThatMap)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..), Form(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Binary form
-------------------------------------------------------------------------------

-- | Binary points.
type BPoint = Point 'Binary

-- | Binary curves.
class (GaloisField q, GaloisField r, Curve 'Binary c e q r)
  => BCurve c e q r where
  {-# MINIMAL a_, b_, h_, p_, r_, x_, y_ #-}
  a_ :: BPoint c e q r -> q       -- ^ Coefficient @A@.
  b_ :: BPoint c e q r -> q       -- ^ Coefficient @B@.
  h_ :: BPoint c e q r -> Integer -- ^ Curve cofactor.
  p_ :: BPoint c e q r -> Integer -- ^ Curve polynomial.
  r_ :: BPoint c e q r -> Integer -- ^ Curve order.
  x_ :: BPoint c e q r -> q       -- ^ Coordinate @X@.
  y_ :: BPoint c e q r -> q       -- ^ Coordinate @Y@.

-- | Binary coordinates.
data Coordinates = Affine
                 | Projective

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Binary affine points.
type BAPoint = BPoint 'Affine

-- | Binary affine curves @y^2 + xy = x^3 + Ax^2 + B@.
class BCurve 'Affine e q r => BACurve e q r where
  {-# MINIMAL gA_ #-}
  gA_ :: BAPoint e q r -- ^ Curve generator.

-- Binary affine curves are elliptic curves.
instance (KnownNat p, BACurve e q (PrimeField p))
  => Curve 'Binary 'Affine e q (PrimeField p) where

  data instance Point 'Binary 'Affine e q (PrimeField p)
    = A q q -- ^ Affine point.
    | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = const 2
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = b_ (witness :: BAPoint e q (PrimeField p))
  {-# INLINE disc #-}

  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = A x <$> yX (witness :: BAPoint e q (PrimeField p)) x
  {-# INLINE pointX #-}

  yX _ x = quad 1 x ((x + a) * x * x + b)
    where
      a = a_ (witness :: BAPoint e q (PrimeField p))
      b = b_ (witness :: BAPoint e q (PrimeField p))
  {-# INLINE yX #-}

-- Binary affine points are groups.
instance (KnownNat p, BACurve e q (PrimeField p))
  => Group (BAPoint e q (PrimeField p)) where

  add p  O      = p
  add O q       = q
  add (A x1 y1) (A x2 y2)
    | xx == 0   = O
    | otherwise = A x3 y3
    where
      a  = a_ (witness :: BAPoint e q (PrimeField p))
      xx = x1 + x2
      yy = y1 + y2
      l  = yy / xx
      x3 = l * (l + 1) + xx + a
      y3 = l * (x1 + x3) + x3 + y1
  {-# INLINE add #-}

  dbl O         = O
  dbl (A x y)
    | x == 0    = O
    | otherwise = A x' y'
    where
      a  = a_ (witness :: BAPoint e q (PrimeField p))
      l  = x + y / x
      l' = l + 1
      x' = l * l' + a
      y' = x * x + l' * x'
  {-# INLINE dbl #-}

  def O       = True
  def (A x y) = ((x + a) * x + y) * x + b + y * y == 0
    where
      a = a_ (witness :: BAPoint e q (PrimeField p))
      b = b_ (witness :: BAPoint e q (PrimeField p))
  {-# INLINE def #-}

  gen = gA_
  {-# INLINE gen #-}

  id = O
  {-# INLINE id #-}

  inv O       = O
  inv (A x y) = A x (x + y)
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Binary affine points are arbitrary.
instance (KnownNat p, BACurve e q (PrimeField p))
  => Arbitrary (BAPoint e q (PrimeField p)) where
  arbitrary = suchThatMap arbitrary pointX

-- Binary affine points are pretty.
instance (KnownNat p, BACurve e q (PrimeField p))
  => Pretty (BAPoint e q (PrimeField p)) where
  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-- Binary affine points are random.
instance (KnownNat p, BACurve e q (PrimeField p))
  => Random (BAPoint e q (PrimeField p)) where
  random g = let (x, g') = random g in case pointX x of
    Just p -> (p, g')
    _      -> random g'
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Projective coordinates
-------------------------------------------------------------------------------

-- | Binary projective points.
type BPPoint = BPoint 'Projective

-- | Binary projective curves @y^2z + xyz = x^3 + Ax^2z + Bz@.
class BCurve 'Projective e q r => BPCurve e q r where
  {-# MINIMAL gP_ #-}
  gP_ :: BPPoint e q r -- ^ Curve generator.

-- Binary projective curves are elliptic curves.
instance (KnownNat p, BPCurve e q (PrimeField p))
  => Curve 'Binary 'Projective e q (PrimeField p) where

  data instance Point 'Binary 'Projective e q (PrimeField p)
    = P q q q -- ^ Projective point.
    deriving (Generic, NFData, Read, Show)

  char = const 2
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = b_ (witness :: BPPoint e q (PrimeField p))
  {-# INLINE disc #-}

  point x y = let p = P x y 1 in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = flip (P x) 1 <$> yX (witness :: BPPoint e q (PrimeField p)) x
  {-# INLINE pointX #-}

  yX _ x = quad 1 x ((x + a) * x * x + b)
    where
      a = a_ (witness :: BPPoint e q (PrimeField p))
      b = b_ (witness :: BPPoint e q (PrimeField p))
  {-# INLINE yX #-}

-- Binary projective points are groups.
instance (KnownNat p, BPCurve e q (PrimeField p))
  => Group (BPPoint e q (PrimeField p)) where

  -- Addition formula add-2008-bl
  add  p           (P  _  _  0) = p
  add (P  _  _  0)  q           = q
  add (P x1 y1 z1) (P x2 y2 z2) = P x3 y3 z3
    where
      y1z2 = y1 * z2
      x1z2 = x1 * z2
      a    = y1z2 + z1 * y2
      b    = x1z2 + z1 * x2
      ab   = a + b
      c    = b * b
      d    = z1 * z2
      e    = b * c
      f    = (a * ab + a_ (witness :: BPPoint e q (PrimeField p)) * c) * d + e
      x3   = b * f
      y3   = c * (a * x1z2 + b * y1z2) + ab * f
      z3   = e * d
  {-# INLINE add #-}

  -- Doubling formula dbl-2008-bl
  dbl (P  _  _  0) = P  0  1  0
  dbl (P x1 y1 z1) = P x3 y3 z3
    where
      a  = x1 * x1
      b  = a + y1 * z1
      c  = x1 * z1
      bc = b + c
      d  = c * c
      e  = b * bc + a_ (witness :: BPPoint e q (PrimeField p)) * d
      x3 = c * e
      y3 = bc * e + a * a * c
      z3 = c * d
  {-# INLINE dbl #-}

  def (P x y z) = ((x + a * z) * x + yz) * x + y * yz + b * z * z * z == 0
    where
      a  = a_ (witness :: BPPoint e q (PrimeField p))
      b  = b_ (witness :: BPPoint e q (PrimeField p))
      yz = y * z
  {-# INLINE def #-}

  gen = gP_
  {-# INLINE gen #-}

  id = P 0 1 0
  {-# INLINE id #-}

  inv (P x y z) = P x (x + y) z
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Binary projective points are arbitrary.
instance (KnownNat p, BPCurve e q (PrimeField p))
  => Arbitrary (BPPoint e q (PrimeField p)) where
  arbitrary = suchThatMap arbitrary pointX

-- Binary projective points are equatable.
instance (KnownNat p, BPCurve e q (PrimeField p)) 
  => Eq (BPPoint e q (PrimeField p)) where
  P x1 y1 z1 == P x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * z2 == x2 * z1 && y1 * z2 == y2 * z1

-- Binary projective points are pretty.
instance (KnownNat p, BPCurve e q (PrimeField p))
  => Pretty (BPPoint e q (PrimeField p)) where
  pretty (P x y z) = pretty (x, y, z)

-- Binary projective points are random.
instance (KnownNat p, BPCurve e q (PrimeField p))
  => Random (BPPoint e q (PrimeField p)) where
  random g = let (x, g') = random g in case pointX x of
    Just p -> (p, g')
    _      -> random g'
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Coordinate transformations
-------------------------------------------------------------------------------

-- | Transform from affine coordinates to projective coordinates.
fromAtoP :: (KnownNat p, BACurve e q (PrimeField p), BPCurve e q (PrimeField p))
  => BAPoint e q (PrimeField p) -> BPPoint e q (PrimeField p)
fromAtoP (A x y) = P x y 1
fromAtoP _       = P 0 1 0
{-# INLINE fromAtoP #-}

-- | Transform from projective coordinates to affine coordinates.
fromPtoA :: (KnownNat p, BACurve e q (PrimeField p), BPCurve e q (PrimeField p))
  => BPPoint e q (PrimeField p) -> BAPoint e q (PrimeField p)
fromPtoA (P _ _ 0) = O
fromPtoA (P x y z) = A (x / z) (y / z)
{-# INLINE fromPtoA #-}
