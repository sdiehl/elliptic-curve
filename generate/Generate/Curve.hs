module Generate.Curve
  ( module Generate.Curve
  ) where

import Protolude

import GHC.Natural (Natural)
import Text.PrettyPrint.Leijen.Text hiding (char)

import Generate.Pretty
import Generate.Types

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

prettyElement :: Element -> Doc
prettyElement (B n)
  = prettyNatural n
prettyElement (E ns)
  = "toE'" <+> align
    (    (if null ns then "[" else "[ ")
    <>   hcat (punctuate "\n, " (map prettyElement ns))
    <$$> "]"
    )
prettyElement (P n)
  = prettyNatural n

prettyField :: Field -> Doc
prettyField (Binary f2m)
  = pretty f2m
prettyField (Extension fq' _ _ _)
  = pretty fq'
prettyField (Prime fq)
  = pretty fq

prettyImport :: Doc
prettyImport
  =    "import Protolude"
  <>   prettyBreak
  <$$> "import Data.Field.Galois"
  <$$> "import GHC.Natural (Natural)"

prettyType :: Text -> Field -> Natural -> Natural -> Doc
prettyType curve (Binary f2m) p r
  =    prettyDocumentation prettyCurve
  <$$> "data" <+> pretty curve
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of points of" <+> prettyCurve)
  <$$> "type" <+> pretty f2m <+> "= Binary P"
  <$$> "type P =" <+> prettyNatural p
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of coefficients of" <+> prettyCurve)
  <$$> "type Fr = Prime R"
  <$$> "type R =" <+> prettyNatural r
  where
    prettyCurve :: Doc
    prettyCurve = pretty curve <+> "curve"
prettyType curve extension@(Extension _ _ _ _) _ _
  =    prettyType' extension
  where
    prettyType' :: Field -> Doc
    prettyType' (Extension fq' p x k)
      =    prettyType' k
      <$$> prettyDocumentation ("Field of points of"
      <+>  pretty curve <+> "curve over" <+> enclose "@" "@" (pretty fq'))
      <$$> "type" <+> pretty fq' <+> "= Extension" <+> pretty p <+> prettyField k
      <$$> "data" <+> pretty p
      <$$> "instance IrreducibleMonic" <+> pretty p <+> prettyField k <+> "where"
      <$$> indent 2
        (    "poly _ =" <+> pretty x
        <$$> prettyInline "poly"
        )
    prettyType' _
      = mempty
prettyType curve (Prime fq) q r
  =    prettyDocumentation prettyCurve
  <$$> "data" <+> pretty curve
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of points of" <+> prettyCurve)
  <$$> "type" <+> pretty fq <+> "= Prime Q"
  <$$> "type Q =" <+> prettyNatural q
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of coefficients of" <+> prettyCurve)
  <$$> "type Fr = Prime R"
  <$$> "type R =" <+> prettyNatural r
  where
    prettyCurve :: Doc
    prettyCurve = pretty curve <+> "curve"
