module Generate.Montgomery.Types
  ( Affine(..)
  , Curve(..)
  , Element(..)
  , Field(..)
  , Parameters(..)
  , Types(..)
  ) where

import Protolude

import Generate.Types

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Curve = Curve
  { types      :: Types
  , parameters :: Parameters
  , affine     :: Affine
  }

data Parameters = Parameters
  { a :: Element
  , b :: Element
  , h :: Integer
  , q :: Integer
  , r :: Integer
  }

data Affine = Affine
  { xA :: Element
  , yA :: Element
  }
