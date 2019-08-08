module Generate.Generate
  ( prettyElement
  , prettyField
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Pretty
import Generate.Types

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

prettyElement :: Element -> Doc
prettyElement (BF n)
  = "BF" <+> prettyInteger n
prettyElement (EF ns)
  = "EF" <+> align
    (    (if null ns then "[" else "[ ")
    <>   hcat (punctuate "\n, " (map prettyElement ns))
    <$$> "]"
    )
prettyElement (PF n)
  = "PF" <+> prettyInteger n

prettyField :: Field -> Doc
prettyField (BinaryField fp p)
  = "BinaryField" <+> prettyText fp <+> prettyInteger p
prettyField (ExtensionField fq fp p s k)
  = align
    (   "ExtensionField" <+> prettyText fq <+> prettyText fp
    <+> prettyText p <+> prettyText s <+> prettyField' k
    )
  where
    prettyField' :: Maybe Field -> Doc
    prettyField' (Just f) = "(Just" <$$> "(" <+> align (prettyField f) <$$> "))"
    prettyField' _        = "Nothing"
prettyField (PrimeField f2m p)
  = "PrimeField" <+> prettyText f2m <+> prettyInteger p
