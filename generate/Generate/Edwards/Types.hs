module Generate.Edwards.Types
  ( module Generate.Edwards.Types
  , module Generate.Types
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
  , d :: Element
  , h :: Integer
  , q :: Integer
  , r :: Integer
  , x :: Element
  , y :: Element
  }
