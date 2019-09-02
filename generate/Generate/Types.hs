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

data Field = Binary Text Natural
           | Extension Text Text Text Text (Maybe Field)
           | Prime Text Natural

data Types = Types
  { base    :: Text
  , curve   :: Text
  , field   :: Field
  , field'  :: Field
  , imports :: Maybe Text
  }
