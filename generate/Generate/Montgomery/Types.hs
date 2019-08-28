module Generate.Montgomery.Types
  ( module Generate.Montgomery.Types
  , module Generate.Types
  ) where

import GHC.Natural (Natural)

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
  , h :: Natural
  , q :: Natural
  , r :: Natural
  , x :: Element
  , y :: Element
  }
