module Generate.Generate
  ( prettyElement
  , prettyField
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Pretty (prettyInteger)
import Generate.Types (Element(..), Field(..))

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

prettyElement :: Element -> Doc
prettyElement (BF n)
  = "BF " <> prettyInteger n
prettyElement (EF ns)
  = "EF " <> align
    (    (if null ns then "[" else "[ ")
    <>   hcat (punctuate "\n, " (map prettyElement ns))
    <$$> "]"
    )
prettyElement (PF n)
  = "PF " <> prettyInteger n

prettyField :: Field -> Doc
prettyField (BinaryField p)
  = "BinaryField " <> prettyInteger p
prettyField (ExtensionField)
  = "ExtensionField"
prettyField (PrimeField p)
  = "PrimeField " <> prettyInteger p
