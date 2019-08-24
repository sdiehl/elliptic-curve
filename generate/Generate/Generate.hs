module Generate.Generate
  ( module Generate.Generate
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Pretty
import Generate.Types

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

prettyElement :: Element -> Doc
prettyElement (B n)
  = "B" <+> prettyInteger n
prettyElement (E ns)
  = "E" <+> align
    (    (if null ns then "[" else "[ ")
    <>   hcat (punctuate "\n, " (map prettyElement ns))
    <$$> "]"
    )
prettyElement (P n)
  = "P" <+> prettyInteger n

prettyField :: Field -> Doc
prettyField (Binary fp p)
  = "Binary" <+> prettyText fp <+> prettyInteger p
prettyField (Extension fq fp p s k)
  = align
    (   "Extension" <+> prettyText fq <+> prettyText fp
    <+> prettyText p <+> prettyText s <+> prettyField' k
    )
  where
    prettyField' :: Maybe Field -> Doc
    prettyField' (Just f) = "(Just" <$$> "(" <+> align (prettyField f) <$$> "))"
    prettyField' _        = "Nothing"
prettyField (Prime f2m p)
  = "Prime" <+> prettyText f2m <+> prettyInteger p
