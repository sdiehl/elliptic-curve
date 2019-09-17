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
  = "B" <+> prettyNatural n
prettyElement (E ns)
  = "E" <+> align
    (    (if null ns then "[" else "[ ")
    <>   hcat (punctuate "\n, " (map prettyElement ns))
    <$$> "]"
    )
prettyElement (P n)
  = "P" <+> prettyNatural n

prettyField :: Field -> Doc
prettyField (Binary f2m)
  = "Binary" <+> prettyText f2m
prettyField (Extension fq p x k)
  = align
    (    "Extension" <+> prettyText fq <+> prettyText p <+> prettyText x <+> "("
    <$$> indent 2 (prettyField k)
    <$$> ")"
    )
prettyField (Prime fq)
  = "Prime" <+> prettyText fq
