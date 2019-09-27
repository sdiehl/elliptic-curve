module Generate.Types
  ( module Generate.Types
  ) where

import Protolude

import GHC.Natural (Natural)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Element = B Natural
             | E [Element]
             | P Natural

data Field = Binary Text
           | Extension Text Text Text Field
           | Prime Text

data Types = Types
  { curve :: Text
  , field :: Field
  }
