module Generate.Types
  ( Element(..)
  , Field(..)
  ) where

import Protolude

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Element = BF Integer
             | EF [Element]
             | PF Integer

data Field = BinaryField Text Integer
           | ExtensionField Text Text Text Text (Maybe Field)
           | PrimeField Text Integer
