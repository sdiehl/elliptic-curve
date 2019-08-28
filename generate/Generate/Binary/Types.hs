module Generate.Binary.Types
  ( module Generate.Binary.Types
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
  , p :: Natural
  , r :: Natural
  , x :: Element
  , y :: Element
  }
