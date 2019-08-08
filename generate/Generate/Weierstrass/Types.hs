module Generate.Weierstrass.Types
  ( Curve(..)
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
  }

data Parameters = Parameters
  { a :: Element
  , b :: Element
  , h :: Integer
  , q :: Integer
  , r :: Integer
  , x :: Element
  , y :: Element
  }
